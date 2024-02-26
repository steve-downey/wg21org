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

  :menu-entry '(?w "WG21 Papers" ((?t "wg21 latex .tex" my-wg21-export-to-latex)
                                  (?p "wg21 latex .pdf" my-wg21-export-to-pdf))))


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




(provide 'ox-wg21latex)
;;; ox-wg21latex.el ends here
