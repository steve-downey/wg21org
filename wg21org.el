;;; wg21org.el --- Write WG21 papers in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steve Downey

;; Author: Steve Downey <sdowney@gmail.com>
;; URL: https://github.com/steve-downey/wg21org
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (org "9.7") (htmlize "1.58") (citeproc "0.9") (org-transclusion "1.4"))
;; Keywords: outlines, text, wp

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

;; The entry point for writing papers with wg21org from an Emacs of
;; your own, rather than through `make':
;;
;;   (add-to-list 'load-path "/path/to/wg21org")
;;   (require 'wg21org)
;;
;; It installs nothing and configures no Emacs.  emacs.d/init.el and
;; emacs.d/export-init.el do that for wg21org's own builds, and load
;; this file to get what a paper needs.  Loading it:
;;
;; - loads the HTML and LaTeX exporters once ox is loaded, and the
;;   reveal.js one when org-re-reveal is installed, so C-c C-e w
;;   exports a paper or a talk;
;; - loads the comparison-table support once Org is loaded, so C-c '
;;   in a cmptblcell that names its block edits that block;
;; - honours a paper's #+bind: lines, see `wg21org-allow-bind-keywords';
;; - defines `wg21org-prepare-export', which every WG21 export command
;;   runs first in the buffer it exports.
;;
;; Everything else the exporters change applies only to a WG21 export:
;; their settings are made in the export's copy of the buffer.  What is
;; global is what they register: their backends, and the link types a
;; paper uses (insert:, delete:, replace:, mark:, sref:, cite-title:).
;;
;; org-transclusion, with its src-lines extension, is needed only to
;; export a paper that transcludes; it is loaded when one is exported.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup wg21org nil
  "Writing WG21 papers in Org."
  :group 'org)

(defconst wg21org-directory
  (file-name-directory (or (macroexp-file-name) buffer-file-name))
  "The directory wg21org is loaded from.")

(defcustom wg21org-allow-bind-keywords t
  "Non-nil means a paper's #+bind: lines set its export options.
Org ignores those lines unless `org-export-allow-bind-keywords' is
set.  When this is non-nil, a WG21 export sets it in its own copy of
the buffer, so a paper exports here as it does through `make', and no
other export is affected.  The values are read, never evaluated."
  :group 'wg21org
  :type 'boolean)

(defun wg21org--load (feature)
  "Load FEATURE from `wg21org-directory'."
  (require feature (expand-file-name (symbol-name feature) wg21org-directory)))

(defun wg21org-enable-transclusion ()
  "Materialize every transclusion in the current Org buffer, or fail.

`org-transclusion-add-all' deliberately continues after a bad link for
interactive use.  Export must be stricter: a stale UUID must not silently
remove an example from a paper.

Calling this again in the same buffer, as when exporting HTML and then
LaTeX in one session, first removes the earlier transclusions so their
keywords are counted and materialized afresh from the current sources."
  (when (derived-mode-p 'org-mode)
    (require 'org-element)
    (require 'org-transclusion)
    (require 'org-transclusion-src-lines)
    (org-transclusion-src-lines-mode +1)
    (when (bound-and-true-p org-transclusion-mode)
      (org-transclusion-mode -1))
    (let ((case-fold-search t)
          (expected
           (length
            (delq
             nil
             (org-element-map (org-element-parse-buffer) 'keyword
               (lambda (keyword)
                 (when (string-equal-ignore-case
                        (org-element-property :key keyword) "transclude")
                   (save-excursion
                     (goto-char (org-element-property :begin keyword))
                     (unless (plist-get
                              (org-transclusion-keyword-string-to-plist)
                              :disable-auto)
                       t)))))))))
      ;; org-transclusion normally calls `org-indent-region' after insertion.
      ;; For source transclusions that also invokes the language indenter and
      ;; changes the spelling shown in the paper.  These directives are at
      ;; column zero, so insertion needs no Org indentation adjustment.
      (cl-letf (((symbol-function 'org-indent-region)
                 (lambda (&rest _) nil)))
        (org-transclusion-mode +1))
      (let ((materialized
             (seq-count
              (lambda (overlay)
                (eq (overlay-get overlay 'face) 'org-transclusion))
              (overlays-in (point-min) (point-max)))))
        (unless (= expected materialized)
          (error "Materialized %d of %d transclusions in %s; check file paths and UUID markers"
                 materialized expected (or buffer-file-name (buffer-name))))))))

(defun wg21org-prepare-export ()
  "Get the current buffer ready to be exported as a WG21 paper.
That is, materialize its transclusions.  Every WG21 export command
runs this first, in the buffer it exports."
  (wg21org-enable-transclusion))

(defun wg21org-allow-bind (backend)
  "Honour #+bind: lines when BACKEND is a WG21 exporter.
Run from `org-export-before-processing-functions', in the export copy
of the buffer; see `wg21org-allow-bind-keywords'."
  (when (and wg21org-allow-bind-keywords
             (org-export-derived-backend-p backend 'wg21-html 'wg21-latex 'wg21-reveal))
    (setq-local org-export-allow-bind-keywords t)))

(with-eval-after-load 'org
  (wg21org--load 'wg21-cmptbl))

(with-eval-after-load 'ox
  (add-hook 'org-export-before-processing-functions #'wg21org-allow-bind)
  (wg21org--load 'ox-wg21html)
  (wg21org--load 'ox-wg21latex)
  (when (locate-library "org-re-reveal")
    (wg21org--load 'ox-wg21reveal)))

(provide 'wg21org)
;;; wg21org.el ends here
