;; wg21-front.el --- front matter of WG21 papers  -*- lexical-binding: t; -*-

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

;; A paper opens with its title, its document block, and its abstract,
;; and only then its table of contents.  Org sets the table of contents
;; ahead of the whole body, abstract included, so each exporter lifts
;; the abstract out of the body and sets it before the contents.

;;; Code:

(require 'org-element)
(require 'ox)

(defun wg21-front-abstract (info)
  "Return the paper's abstract block, if it comes before any headline.
That is a #+begin_abstract block in the text that opens the paper.
INFO is a plist holding export options."
  (org-element-map (plist-get info :parse-tree) 'special-block
    (lambda (block)
      (and (string= (downcase (org-element-property :type block)) "abstract")
           (eq (org-element-type (org-element-parent block)) 'section)
           (eq (org-element-type (org-element-parent (org-element-parent block)))
               'org-data)
           block))
    info t))

(defun wg21-front-lift-abstract (contents info)
  "Split the abstract out of CONTENTS, the transcoded body.
Return (ABSTRACT . REST), where ABSTRACT is the abstract as exported,
or nil when the paper has none, and REST is CONTENTS without it.
INFO is a plist holding export options."
  (let* ((block (wg21-front-abstract info))
         ;; Without the blank lines after it, which the body may not keep.
         (abstract (and block (string-trim-right (org-export-data block info))))
         (at (and abstract (not (string-empty-p abstract))
                  (string-search abstract contents))))
    (if at
        (cons abstract (concat (substring contents 0 at)
                               (substring contents (+ at (length abstract)))))
      (cons nil contents))))

(provide 'wg21-front)
;;; wg21-front.el ends here
