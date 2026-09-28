;; ox-wg21latex.el --- org exporter for WG21 papers in Latex format  -*- lexical-binding: t; -*-

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
(require 'wg21-links
         (expand-file-name "wg21-links"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'wg21-git
         (expand-file-name "wg21-git"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'wg21-cmptbl
         (expand-file-name "wg21-cmptbl"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'wg21-cite
         (expand-file-name "wg21-cite"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

;; Loaded when present; the export falls back to plain verbatim code.
(require 'engrave-faces nil t)

(defun my-latex-special-block (special-block contents info)
  "Process my special block.  SPECIAL-BLOCK CONTENTS INFO.
Block names are case-insensitive in Org, but the environment named
after one is not, so #+BEGIN_ABSTRACT becomes \\begin{abstract}."
  (org-element-put-property special-block :type
                            (downcase (org-element-property :type special-block)))
  (if (string= (org-element-property :type special-block) "cmptbl")
      (wg21-latex-cmptbl special-block info)
    (wg21-latex--guard-environment
     (org-element-property :type special-block)
     (org-latex-special-block special-block contents info))))

(defun wg21-latex--guard-environment (name latex)
  "Set LATEX, the environment NAME, in a wgblock environment.
wgblock, from wg21org-preamble.tex, uses NAME when the document
defines it, and sets the contents plainly when not, so a block the
paper's class lacks does not stop the build.  An environment with
options after \\begin{NAME} is left as it is."
  (let ((begin (format "\\begin{%s}\n" name))
        (end (format "\\end{%s}" name)))
    (if (and (string-prefix-p begin latex)
             (string-match (concat (regexp-quote end) "\\([ \t\n]*\\)\\'") latex))
        (concat (format "\\begin{wgblock}{%s}\n" name)
                (substring latex (length begin) (match-beginning 0))
                "\\end{wgblock}"
                (match-string 1 latex))
      latex)))

;;; Comparison tables

;; Rows are grouped by wg21-cmptbl.el, shared with the HTML exporter,
;; and set in the wgcmptbl environment from wg21org-preamble.tex.

(defun wg21-latex--cmptbl-cells (row columns info)
  "Return the cells of ROW as the body of a table row, without its end.
Each cell's contents are in a wgcmptblcell environment, which lets a
breakable code box sit in a longtable cell.  COLUMNS is the width of
the table.  INFO is the export plist."
  (let ((cells (mapcar (lambda (cell)
                         (format "\\begin{wgcmptblcell}\n%s\n\\end{wgcmptblcell}"
                                 (org-trim (org-export-data (org-element-contents cell) info))))
                       row)))
    (mapconcat #'identity
               (append cells (make-list (max 0 (- columns (length cells))) ""))
               "\n&\n")))

(defun wg21-latex--cmptbl-head (row columns info)
  "Return ROW as a centred header row.
COLUMNS is the width of the table.  INFO is the export plist."
  (let ((column 0))
    (mapconcat
     (lambda (cell)
       (setq column (1+ column))
       (format "\\multicolumn{1}{%sc%s}{%s}"
               (if (= column 1) "@{}" "")
               (if (= column columns) "@{}" "")
               (org-trim (org-export-data (org-element-contents cell) info))))
     row " & ")))

(defun wg21-latex--cmptbl-note (row columns info)
  "Return ROW, a single element that is not a cell, spanning the table.
COLUMNS is the width of the table.  INFO is the export plist."
  (let ((note (org-trim (org-export-data (car row) info))))
    (unless (string-empty-p note)
      (format "\\multicolumn{%d}{@{}p{\\linewidth}@{}}{%s}" columns note))))

(defun wg21-latex-cmptbl (cmptbl info)
  "Transcode the comparison table CMPTBL into a wgcmptbl environment.
INFO is a plist holding export options."
  (let* ((rows (wg21-cmptbl-rows cmptbl))
         (columns (apply #'max 2 (mapcar (lambda (row)
                                           (if (wg21-cmptbl-cell-p (car row)) (length row) 1))
                                         rows)))
         (head (and (wg21-cmptbl-header-p (car rows)) (pop rows)))
         (body (delq nil
                     (mapcar (lambda (row)
                               (if (wg21-cmptbl-cell-p (car row))
                                   (wg21-latex--cmptbl-cells row columns info)
                                 (wg21-latex--cmptbl-note row columns info)))
                             rows))))
    (concat
     "\\begin{wgcmptbl}\n\\toprule\n"
     (when head
       (concat (wg21-latex--cmptbl-head head columns info)
               " \\\\\n\\midrule\n\\endhead\n"))
     "\\bottomrule\n\\endlastfoot\n"
     (mapconcat (lambda (row) (concat row " \\\\")) body "\n\\midrule\n")
     "\n\\end{wgcmptbl}\n")))

;;; Code

(defcustom wg21-latex-engraved-theme "modus-operandi-tinted"
  "Emacs theme whose faces colour code, set per paper by #+LATEX_ENGRAVED_THEME.
Code is fontified by its major mode and set with engrave-faces, so it
looks as it does in the editor.  A name, as Org reads the keyword."
  :group 'my-export-wg21
  :type 'string)

;; engrave-faces only takes theme colours for faces in its preset list;
;; others keep the colours of the running Emacs.  Add rainbow-delimiters,
;; whose slugs become TeX macro names, so letters only.
(with-eval-after-load 'engrave-faces
  (dolist (face (append (mapcar (lambda (depth)
                                  (cons (intern (format "rainbow-delimiters-depth-%d-face" depth))
                                        (format "rd%c" (+ ?a depth -1))))
                                (number-sequence 1 9))
                        '((rainbow-delimiters-unmatched-face . "rdunmatched")
                          (rainbow-delimiters-mismatched-face . "rdmismatched"))))
    (unless (assq (car face) engrave-faces-current-preset-style)
      (add-to-list 'engrave-faces-current-preset-style
                   (list (car face) :short (cdr face) :slug (cdr face))
                   t))))

;;; Preamble

(defconst wg21-latex-directory
  (file-name-directory (or (macroexp-file-name) buffer-file-name))
  "The directory of this exporter, holding its default preamble.")

(defcustom wg21-latex-preamble "wg21org-preamble.tex"
  "Preamble fragment copied into every paper.
A paper can name another with #+WG21_LATEX_PREAMBLE.  A relative
name is looked up next to the paper, then next to this
exporter.  Copying it, rather than \\input, keeps the exported .tex
compilable wherever it is."
  :group 'my-export-wg21
  :type 'string)

(defun wg21-latex--preamble (info)
  "Return the contents of the paper's preamble fragment.
INFO is a plist holding export options."
  (let* ((name (plist-get info :wg21-latex-preamble))
         (input (plist-get info :input-file))
         (file (seq-find #'file-readable-p
                         (delq nil
                               (list (and input (expand-file-name
                                                 name (file-name-directory input)))
                                     (expand-file-name name wg21-latex-directory))))))
    (unless file
      (user-error "LaTeX preamble %s not found" name))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

;;; Title block and source links

(defun wg21-latex--url (url)
  "Return URL protected for use in \\href."
  (replace-regexp-in-string "[%#\\\\]" "\\\\\\&" url))

(defun wg21-latex--title-block (info)
  "Return the title and the table of document metadata.
INFO is a plist holding export options."
  (let* ((text (lambda (value) (org-latex-plain-text value info)))
         (author (org-export-data (plist-get info :author) info))
         (email (org-export-data (plist-get info :email) info))
         (git (wg21-git-metadata info))
         (repo (plist-get git :repo))
         (file (plist-get git :file))
         (url (plist-get git :url))
         (version (plist-get git :version))
         (row (lambda (label value)
                (format "\\wgmetalabel{%s} & \\wgmetavalue{%s} \\\\\n" label value))))
    (concat
     "\\begin{flushleft}\n"
     (format "{\\sffamily\\bfseries\\LARGE %s\\par}\n\\bigskip\n"
             (org-export-data (plist-get info :title) info))
     "\\begin{tabular}{@{}ll@{}}\n"
     (funcall row "Document \\#:" (org-export-data (plist-get info :docnumber) info))
     (funcall row "Date:" (org-export-data (org-export-get-date info) info))
     (funcall row "Audience:" (org-export-data (plist-get info :audience) info))
     (funcall row "Reply-to:"
              (if (string-empty-p email) author
                (format "%s \\textless\\href{mailto:%s}{%s}\\textgreater"
                        author (wg21-latex--url email) (funcall text email))))
     (when repo
       (funcall row "Source:" (format "\\href{%s}{%s}" (wg21-latex--url repo)
                                      (funcall text repo))))
     (when file
       (funcall row "" (if url
                           (format "\\href{%s}{%s}" (wg21-latex--url url) (funcall text file))
                         (funcall text file))))
     (when version
       (funcall row "" (format "\\texttt{%s}" (funcall text version))))
     "\\end{tabular}\n\\end{flushleft}\n\\bigskip\n")))

(defun wg21-latex-footnote-reference (footnote-reference contents info)
  "Transcode FOOTNOTE-REFERENCE as \\wgfootnote, not \\footnote.
\\wgfootnote, from wg21org-preamble.tex, works whether the paper has
the \\footnote command or, from common.tex, a footnote environment.
CONTENTS is nil.  INFO is a plist holding export options."
  (replace-regexp-in-string
   "\\\\footnote{" "\\wgfootnote{"
   (org-latex-footnote-reference footnote-reference contents info)
   t t))

(defun wg21-latex-headline (headline contents info)
  "Transcode HEADLINE, with a margin link to its line in the Org source.
CONTENTS is the headline's contents.  INFO is a plist holding export
options."
  (let ((latex (org-latex-headline headline contents info))
        (source (and (not (org-export-low-level-p headline info))
                     (wg21-git-headline-url headline info))))
    (if (and latex source
             (string-match "\\`[^\n]*\n\\(?:\\\\label{[^}\n]*}\n\\)?" latex))
        (concat (match-string 0 latex)
                (format "\\wgsourcelink{%s}%%\n" (wg21-latex--url source))
                (substring latex (match-end 0)))
      latex)))

(defcustom wg21-document-number "Dnnnn"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-audience "WG21"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-toc-command "\\wgtableofcontents\n\n"
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
    (:source_repo "SOURCE_REPO" nil "" nil)
    (:source_file "SOURCE_FILE" nil "" parse)
    (:source_version "SOURCE_VERSION" nil "" parse)
    (:git_commit "GIT_COMMIT" nil "" parse)
    (:wg21-latex-preamble "WG21_LATEX_PREAMBLE" nil wg21-latex-preamble t)
    ;; Only an address the paper gives, not the exporting user's.
    (:email "EMAIL" nil "" t)
    ;; Code set as in the editor; see `wg21-latex-engraved-theme'.
    (:latex-src-block-backend nil nil
     (if (featurep 'engrave-faces) 'engraved org-latex-src-block-backend))
    (:latex-engraved-theme "LATEX_ENGRAVED_THEME" nil wg21-latex-engraved-theme)
    (:wg21-toc-command nil nil wg21-toc-command))

  :translate-alist '((special-block . my-latex-special-block)
                     (headline . wg21-latex-headline)
                     (footnote-reference . wg21-latex-footnote-reference)
                     (template . my-wg21-latex-template))

  :filters-alist '((:filter-parse-tree . wg21-cite-drop-empty-bibliography))

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
     ;; WG21 preamble, after the paper's own so the paper's definitions win.
     (wg21-latex--preamble info)
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title and document metadata.
     (and (plist-get info :with-title)
          (not (string= "" title))
          (wg21-latex--title-block info))
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
  (org-export-to-buffer 'wg21-latex "*Org WG21 LaTeX Export*"
    async subtreep visible-only body-only ext-plist (lambda () (if (fboundp 'LaTeX-mode) (LaTeX-mode) (latex-mode)))))

;;;###autoload
(defun my-wg21-convert-region-to-latex ()
  "Assume the current region has Org syntax, and convert it to LaTeX.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an LaTeX buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'wg21-latex))

(defalias 'my-wg21-export-region-to-latex #'my-wg21-convert-region-to-latex)

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
