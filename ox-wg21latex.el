(require 'ox-latex)

;;; Code:
(defun my-latex-cmptblcell-block (special-block contents info)
  "Process my cmptblcell block.  SPECIAL-BLOCK CONTENTS INFO."
  (let ((side (org-element-property :parameters special-block)))
    (if (string= (downcase side) "before")
        (concat (org-latex-special-block special-block contents info) "\n &\n")
      (concat (org-latex-special-block special-block contents info) " \\\\ \\hline\n")
      )))


(defun my-latex-special-block (special-block contents info)
  "Process my special block.  SPECIAL-BLOCK CONTENTS INFO."
   (let ((block-type (org-element-property :type special-block)))
     (if (string= (downcase block-type) "cmptblcell")
         (progn (my-latex-cmptblcell-block special-block contents info))
       (org-latex-special-block special-block contents info))))


(defun my-wg21-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'wg21-latex file
      async subtreep visible-only body-only ext-plist)))

(defun my-wg21-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as PDF."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'wg21-latex file
      async subtreep visible-only body-only ext-plist
      #'org-latex-compile)))

(org-export-define-derived-backend 'wg21-latex 'latex
  :translate-alist '((special-block . my-latex-special-block))
  :menu-entry '(?w "WG21 Papers" ((?t "wg21 latex .tex" my-wg21-export-to-latex)
                                  (?p "wg21 latex .pdf" my-wg21-export-to-pdf))))


(provide 'ox-wg21latex)
;;; ox-wg21latex.el ends here
