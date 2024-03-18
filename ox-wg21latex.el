;; ox-wg21latex.el --- org exporter for WG21 papers in Latex format

;; Copyright (C) 2024 Steve Downey

;; Author: Steve Downey <sdowney@gmail.com>

;; URL:

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

;;; Code:

(require 'ox-latex)

(defun my-latex-cmptblcell-block (special-block contents info)
  "Process my cmptblcell block.  SPECIAL-BLOCK CONTENTS INFO."
  (let ((side (org-element-property :parameters special-block)))
    (if (string= (downcase side) "before")
        (concat (org-latex-special-block special-block contents info) "\n &\n")
      (concat (org-latex-special-block special-block contents info) " \\\\ \\midrule\n")
      )))


(defun my-latex-special-block (special-block contents info)
  "Process my special block.  SPECIAL-BLOCK CONTENTS INFO."
   (let ((block-type (org-element-property :type special-block)))
     (if (string= (downcase block-type) "cmptblcell")
         (progn (my-latex-cmptblcell-block special-block contents info))
       (org-latex-special-block special-block contents info))))

(defcustom wg21-document-number "Dnnnn"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-audience "WG21"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-toc-command "\\tableofcontents*\n\n"
  "LaTeX command to set the table of contents, list of figures, etc.
This command only applies to the table of contents generated with the
toc:t, toc:1, toc:2, toc:3, ... options, not to those generated with
the #+TOC keyword."
  :group 'my-export-wg21
  :type 'string)

(org-export-define-derived-backend 'wg21-latex 'latex
  :options-alist
  '((:docnumber "DOCNUMBER" nil wg21-document-number nil)
    (:audience "AUDIENCE" nil wg21-audience nil)
    (:wg21-toc-command nil nil wg21-toc-command))

  :translate-alist '((special-block . my-latex-special-block)
                     (template . my-wg21-latex-template))

  :menu-entry '(?w "WG21 Papers"
                   ((?L "As LaTeX buffer" my-wg21-export-as-latex)
	                (?l "As LaTeX file" my-wg21-export-to-latex)
	                (?p "As PDF file" my-wg21-export-to-pdf)
	                (?O "As PDF file and open"
	                    (lambda (a s v b)
	                      (if a (my-wg21-export-to-pdf t s v b)
		                    (org-open-file (my-wg21-export-to-pdf nil s v b))))))))


(defun my-wg21-latex-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	    (spec (org-latex--format-spec info)))
    (concat
     ;; Timestamp.
     (and (plist-get info :time-stamp-file)
	      (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	     (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			            (let ((auth (plist-get info :author)))
			              (and auth (org-export-data auth info)))))
	       (email (and (plist-get info :with-email)
		               (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	          (format "\\author{%s\\thanks{%s}}\n" author email))
	         ((or author email) (format "\\author{%s}\n" (or author email)))))
     ;; Date.
     ;; LaTeX displays today's date by default. One can override this by
     ;; inserting \date{} for no date, or \date{string} with any other
     ;; string to be displayed as the date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
	        (formatted-subtitle
	         (when subtitle
	           (format (plist-get info :latex-subtitle-format)
		               (org-export-data subtitle info))))
	        (separate (plist-get info :latex-subtitle-separate)))
       (concat
	    (format "\\title{%s%s}\n" title
		        (if separate "" (or formatted-subtitle "")))
	    (when (and separate subtitle)
	      (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; engrave-faces-latex preamble
     (when (and (eq (plist-get info :latex-src-block-backend) 'engraved)
                (org-element-map (plist-get info :parse-tree)
                    '(src-block inline-src-block) #'identity
                    info t))
       (org-latex-generate-engraved-preamble info))
     ;; Docblock vars
     (let ((audience (plist-get info :audience))
           (docnumber (plist-get info :docnumber))
           (email (plist-get info :email)))
       (concat "
\\docnumber{" (org-export-data docnumber info) "}
\\email{" (org-export-data email info) "}
\\audience{" (org-export-data audience info) "}\n"))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
	    (cond ((not (plist-get info :with-title)) nil)
	          ((string= "" title) nil)
	          ((not (stringp command)) nil)
	          ((string-match "\\(?:[^%]\\|^\\)%s" command)
	           (format command title))
	          (t command))))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	     (concat (when (integerp depth)
		           (format "\\setcounter{tocdepth}{%d}\n" depth))
		         (plist-get info :wg21-toc-command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
	      (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))


(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-classes
                '("memoir" "\\documentclass{memoir}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))



;;; End-user functions

;;;###autoload
(defun my-wg21-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a LaTeX buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org WG21 LaTeX Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'latex "*Org WG21 LaTeX Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun my-wg21-convert-region-to-latex ()
  "Assume the current region has Org syntax, and convert it to LaTeX.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an LaTeX buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'wg21-latex))

(defalias 'org-export-region-to-latex #'my-wg21-latex-convert-region-to-latex)

;;;###autoload
(defun my-wg21-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a LaTeX file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'wg21-latex outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun my-wg21-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'wg21-latex outfile
      async subtreep visible-only body-only ext-plist
      #'org-latex-compile)))


(provide 'ox-wg21latex)
;;; ox-wg21latex.el ends here
