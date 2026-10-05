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
;;   [[replace:new text][old text]] substituted text
;;   [[mark:][important text]] highlighted text
;;
;; The description is the text; the link path is ignored.  In HTML
;; they export as <ins> and <del>; in LaTeX as \added and \removed from
;; stdtex/macros.tex.

;;; Code:

(require 'ol)
(require 'ox)
(require 'org-element)
(require 'wg21-wording
         (expand-file-name "wg21-wording"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

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

(defun wg21-links--replace-export (replacement original backend info)
  "Export ORIGINAL replaced by REPLACEMENT for BACKEND."
  (setq original (or original ""))
  (setq replacement (org-link-decode replacement))
  (setq replacement
        (org-export-data
         (org-element-parse-secondary-string
          replacement (org-element-restriction 'paragraph))
         info))
  (cond
   ((org-export-derived-backend-p backend 'html)
    (format "<del>%s</del><ins>%s</ins>" original replacement))
   ((org-export-derived-backend-p backend 'latex)
    (format "\\removed{%s}\\added{%s}" original replacement))
   (t (concat original replacement))))

(defun wg21-links--mark-export (path description backend _info)
  "Export highlighted DESCRIPTION, falling back to PATH, for BACKEND."
  (let ((text (or description (org-link-decode path))))
    (cond
     ((org-export-derived-backend-p backend 'html)
      (format "<mark>%s</mark>" text))
     ((org-export-derived-backend-p backend 'latex)
      (format "\\wgmark{%s}" text))
     (t text))))

(org-link-set-parameters "replace" :export #'wg21-links--replace-export)
(org-link-set-parameters "mark" :export #'wg21-links--mark-export)

(defun wg21-links--sref-export (path description backend info)
  "Export a stable-name reference PATH with optional DESCRIPTION."
  (let* ((parts (split-string path "/" t))
         (name (car parts))
         (pnum (cadr parts))
         (text (or description
                   (concat "[" name "]" (if pnum (concat "/" pnum) ""))))
         (local (wg21-local-stable-name-p name info))
         (href (if local
                   (concat "#" name (if pnum (concat "-" pnum) ""))
                 (concat "https://eel.is/c++draft/" name
                         (if pnum (concat "#" pnum) "")))))
    (cond
     ((org-export-derived-backend-p backend 'html)
      (format "<a class=\"sref\" href=\"%s\">%s</a>" href text))
     ((org-export-derived-backend-p backend 'latex)
      (if local
          (if pnum
              (format "\\hyperlink{%s-%s}{%s}" name pnum text)
            (format "\\hyperref[%s]{%s}" name text))
        (format "\\href{%s}{%s}" href text)))
     (t text))))

;; [[sref:basic.life]] and [[sref:basic.life/2.1][custom text]].
(org-link-set-parameters "sref" :export #'wg21-links--sref-export)

(provide 'wg21-links)
;;; wg21-links.el ends here
