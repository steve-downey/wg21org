;; wg21-cmptbl.el --- comparison table structure for WG21 papers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steve Downey

;; Author: Steve Downey <sdowney@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; A comparison table is a #+begin_cmptbl block holding
;; #+begin_cmptblcell blocks, whose parameter names their column,
;; `before' or `after':
;;
;;   #+begin_cmptbl
;;   #+begin_cmptblcell before
;;   *Before*
;;   #+end_cmptblcell
;;   #+begin_cmptblcell after
;;   *After*
;;   #+end_cmptblcell
;;   ...
;;   #+end_cmptbl
;;
;; Each `before' cell starts a row.  A first row with no code in it is
;; the header.  More compact, and more general, tables can declare their
;; headings and contain source blocks directly:
;;
;;   #+caption: Two implementations
;;   #+attr_wg21: :columns 60 40
;;   #+begin_cmptbl :headers "Portable | Native"
;;   #+begin_src C++
;;   portable();
;;   #+end_src
;;   #+begin_src C++
;;   native();
;;   #+end_src
;;   #+end_cmptbl
;;
;; Direct code blocks fill rows from left to right.  Explicit cmptblcell
;; blocks remain useful when a cell needs arbitrary Org contents.

;;; Code:

(require 'org-element)
(require 'seq)

(defun wg21-cmptbl--arguments (cmptbl)
  "Return CMPTBL's parameters, split with ordinary Emacs quoting."
  (split-string-and-unquote
   (or (org-element-property :parameters cmptbl) "")))

(defun wg21-cmptbl--option (cmptbl name)
  "Return option NAME from CMPTBL's parameters, or nil."
  (let ((arguments (wg21-cmptbl--arguments cmptbl)) value)
    (while arguments
      (let ((argument (pop arguments)))
        (when (string= argument (concat ":" name))
          (setq value (pop arguments)
                arguments nil))))
    value))

(defun wg21-cmptbl-headers (cmptbl)
  "Return CMPTBL's declared column headings, or nil.
Headings are separated by vertical bars in the :headers block option."
  (when-let* ((headers (wg21-cmptbl--option cmptbl "headers")))
    (mapcar #'string-trim (split-string headers "|" t))))

(defun wg21-cmptbl-widths (cmptbl)
  "Return CMPTBL's percentage column widths, or nil.
Widths use the same native #+ATTR_WG21: :columns attribute as ordinary
Org tables.  There must be one positive integer width per column."
  (when-let* ((columns (org-export-read-attribute :attr_wg21 cmptbl :columns))
              (widths (split-string columns "[ \t]+" t)))
    (unless (and (seq-every-p
                  (lambda (width) (string-match-p "\\`[1-9][0-9]*\\'" width))
                  widths)
                 (= 100 (apply #'+ (mapcar #'string-to-number widths))))
      (user-error "Comparison-table widths must be positive integers totaling 100: %s"
                  columns))
    widths))

(defun wg21-cmptbl-cell-p (element)
  "Non-nil if ELEMENT is a cmptblcell special block."
  (and (eq (org-element-type element) 'special-block)
       (string= (downcase (org-element-property :type element)) "cmptblcell")))

(defun wg21-cmptbl-direct-cell-p (element)
  "Non-nil if ELEMENT can be used directly as a compact table cell."
  (memq (org-element-type element)
        '(src-block example-block fixed-width table verse-block)))

(defun wg21-cmptbl-any-cell-p (element)
  "Non-nil if ELEMENT is an explicit or compact comparison-table cell."
  (or (wg21-cmptbl-cell-p element) (wg21-cmptbl-direct-cell-p element)))

(defun wg21-cmptbl-side (cell)
  "Return the column name of CELL, from its parameter, or nil."
  (let ((side (org-element-property :parameters cell)))
    (and side (downcase (org-trim side)))))

(defun wg21-cmptbl-rows (cmptbl)
  "Group the children of CMPTBL into rows.
Each row is a list of cells.  Anything that is not a cell is a row of
its own, a list of just that element, so it is shown rather than dropped."
  (let* ((headers (wg21-cmptbl-headers cmptbl))
         (widths (wg21-cmptbl-widths cmptbl))
         (columns (cond (headers (length headers))
                        (widths (length widths))
                        (t 2)))
         first-side rows row)
    (dolist (child (org-element-contents cmptbl))
      (cond
       ((not (wg21-cmptbl-any-cell-p child))
        (when row (push (nreverse row) rows) (setq row nil))
        (push (list child) rows))
       ((or (and (wg21-cmptbl-cell-p child)
                 (let ((side (wg21-cmptbl-side child)))
                   (when (and side (not first-side)) (setq first-side side))
                   (and row side (equal side first-side))))
            (>= (length row) columns))
        (when row (push (nreverse row) rows))
        (setq row (list child)))
       (t (push child row))))
    (when row (push (nreverse row) rows))
    (nreverse rows)))

(defun wg21-cmptbl-header-p (row)
  "Non-nil if ROW, a list of cells, holds only prose, so is a header."
  (and (consp row)
       (seq-every-p #'wg21-cmptbl-cell-p row)
       (not (org-element-map row '(src-block example-block fixed-width table)
              #'identity nil t))))

(defun wg21-cmptbl-cell-contents (cell)
  "Return the Org data exported for comparison-table CELL."
  (if (wg21-cmptbl-cell-p cell) (org-element-contents cell) cell))

(defun wg21-cmptbl-column-class (cell column headers)
  "Return a CSS-safe column name for CELL at COLUMN, using HEADERS."
  (or (and (wg21-cmptbl-cell-p cell)
           (wg21-cmptbl-side cell))
      (when-let* ((header (nth column headers)))
        (let ((slug (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" header))))
          (string-trim slug "-+" "-+")))
      (format "column-%d" (1+ column))))

(provide 'wg21-cmptbl)
;;; wg21-cmptbl.el ends here
