;; wg21-links.el --- org link types for WG21 papers  -*- lexical-binding: t; -*-

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

;; Inline wording changes, shared by the HTML and LaTeX exporters:
;;
;;   [[insert:][new text]]   added text
;;   [[delete:][old text]]   removed text
;;
;; The description is the text; the link path is ignored.  In HTML
;; they export as <ins> and <del>; in LaTeX as \added and \removed from
;; stdtex/macros.tex.

;;; Code:

(require 'ol)
(require 'ox)

(defun wg21-links--export (html-tag latex-macro)
  "Return a link export function for an inline wording change.
HTML-TAG is the element used for HTML, LATEX-MACRO the command used
for LaTeX."
  (lambda (path description backend _info)
    (let ((text (or description path)))
      (cond
       ((org-export-derived-backend-p backend 'html)
        (format "<%s>%s</%s>" html-tag text html-tag))
       ((org-export-derived-backend-p backend 'latex)
        (format "\\%s{%s}" latex-macro text))
       (t text)))))

(org-link-set-parameters "insert" :export (wg21-links--export "ins" "added"))
(org-link-set-parameters "delete" :export (wg21-links--export "del" "removed"))

(provide 'wg21-links)
;;; wg21-links.el ends here
