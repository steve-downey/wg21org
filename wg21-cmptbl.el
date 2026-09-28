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
;; the header.  This file groups the cells into rows; the HTML and LaTeX
;; exporters each lay the rows out.

;;; Code:

(require 'org-element)
(require 'seq)

(defun wg21-cmptbl-cell-p (element)
  "Non-nil if ELEMENT is a cmptblcell special block."
  (and (eq (org-element-type element) 'special-block)
       (string= (downcase (org-element-property :type element)) "cmptblcell")))

(defun wg21-cmptbl-side (cell)
  "Return the column name of CELL, from its parameter, or nil."
  (let ((side (org-element-property :parameters cell)))
    (and side (downcase (org-trim side)))))

(defun wg21-cmptbl-rows (cmptbl)
  "Group the children of CMPTBL into rows.
Each row is a list of cells.  Anything that is not a cell is a row of
its own, a list of just that element, so it is shown rather than dropped."
  (let (rows row)
    (dolist (child (org-element-contents cmptbl))
      (cond
       ((not (wg21-cmptbl-cell-p child))
        (when row (push (nreverse row) rows) (setq row nil))
        (push (list child) rows))
       ((or (equal (wg21-cmptbl-side child) "before")
            (>= (length row) 2))
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

(provide 'wg21-cmptbl)
;;; wg21-cmptbl.el ends here
