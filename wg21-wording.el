;; wg21-wording.el --- proposed wording in WG21 papers  -*- lexical-binding: t; -*-

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

;; Proposed wording is a #+begin_wording block, the wording environment
;; of stdtex/paper_macros.tex, which numbers its sections the way the
;; working draft does.  In wording, colour means an edit: green inserted,
;; red removed.  So code there is not syntax highlighted, which would
;; put the same colours on text that is not an edit; each exporter sets
;; it the way the working draft sets code.

;;; Code:

(require 'org-element)

(defun wg21-wording-p (element)
  "Non-nil if ELEMENT is inside a #+begin_wording block."
  (let ((ancestor (org-element-parent element)) found)
    (while (and ancestor (not found))
      (setq found (and (eq (org-element-type ancestor) 'special-block)
                       (string= (downcase (org-element-property :type ancestor))
                                "wording"))
            ancestor (org-element-parent ancestor)))
    found))

(provide 'wg21-wording)
;;; wg21-wording.el ends here
