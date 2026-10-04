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

(require 'cl-lib)
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
(require 'wg21-wording
         (expand-file-name "wg21-wording"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'wg21-front
         (expand-file-name "wg21-front"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

;; Loaded when present; the export falls back to plain verbatim code.
(require 'engrave-faces nil t)

(defun my-latex-special-block (special-block contents info)
  "Process my special block.  SPECIAL-BLOCK CONTENTS INFO.
Block names are case-insensitive in Org, but the environment named
after one is not, so #+BEGIN_ABSTRACT becomes \\begin{abstract}."
  (org-element-put-property special-block :type
                            (downcase (org-element-property :type special-block)))
  (let ((type (org-element-property :type special-block)))
    (cond
     ((string= type "cmptbl") (wg21-latex-cmptbl special-block info))
     ((string= type "pnum")
      (concat "\\pnum\n" contents))
     ((member type '("codeblock" "itemdecl"))
      ;; listings environments find their end marker by scanning the input;
      ;; hiding it behind wgblock makes the first block consume the paper.
      (format "\\begin{%s}\n%s\\end{%s}\n"
              type
              (replace-regexp-in-string
               "\\\\ref{\\([^}]+\\)}" "[\\1]"
               (wg21-special-block-raw-contents special-block))
              type))
     (t (wg21-latex--guard-environment
         type (org-latex-special-block special-block contents info))))))

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

(defun wg21-latex-headline (headline contents info)
  "Export HEADLINE, wrapping a generated wording root around its subtree."
  (let ((latex (org-latex-headline headline contents info)))
    (if (org-element-property :WG21_WORDING headline)
        (concat "\\begin{wgwording}\n" latex "\\end{wgwording}\n")
      latex)))

(defun wg21-latex-table (table contents info)
  "Export TABLE, applying target-neutral WG21 column proportions."
  (let ((widths (wg21-table-columns table)))
    (if (not widths)
        (org-latex-table table contents info)
      (let ((copy (org-element-copy table)))
        (org-element-put-property
         copy :attr_latex
         (list (concat ":environment longtable :align @{}"
                       (mapconcat (lambda (width)
                                    (format "p{.%s\\linewidth}" width))
                                  widths "")
                       "@{}")))
        (org-latex-table copy contents info)))))

;;; Wording

(defconst wg21-latex-wording-code-languages '("c++" "cpp" "c")
  "Languages set as C++ in wording; other code is set as output.")

(defun wg21-latex-src-block (src-block contents info)
  "Transcode SRC-BLOCK, as the working draft sets code when in wording.
In a #+begin_wording block, C++ goes in stdtex's codeblock and other
code in its outputblock, from the common.tex prolog, instead of being
highlighted; see wg21-wording.el.  The code is passed as it is, so
@\\added{...}@ and @\\removed{...}@ mark edits inside it.  CONTENTS
is nil.  INFO is the export plist."
  (if (wg21-wording-p src-block)
      (let ((code (car (org-export-unravel-code src-block)))
            (environment (if (member (downcase (or (org-element-property :language src-block) ""))
                                     wg21-latex-wording-code-languages)
                             "codeblock"
                           "outputblock")))
        (format "\\begin{%s}\n%s%s\\end{%s}\n"
                environment code
                (if (string-suffix-p "\n" code) "" "\n")
                environment))
    (org-latex-src-block src-block contents info)))

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

(defcustom wg21-latex-prolog "common.tex"
  "The prolog every paper starts from, set per paper by #+WG21_LATEX_PROLOG.
By default the working draft's macros and layout, from common.tex and
the stdtex files it reads, which also give wording its section
numbers and code its draft formatting.  \"none\" means no prolog.  A
relative name is looked up next to the paper, then next to this
exporter.  It is copied into the paper, with the files it \\inputs,
before the paper's own #+LATEX_HEADER lines."
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-latex-class "memoir"
  "Document class of a paper that names none; common.tex needs memoir."
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-latex-class-options "[a4paper,10pt,oneside,openany,final,article]"
  "Class options of a paper that names none."
  :group 'my-export-wg21
  :type 'string)

(defun wg21-latex--find-file (name info)
  "Return the file NAME, looked up next to the paper and then this exporter.
INFO is a plist holding export options."
  (let ((input (plist-get info :input-file)))
    (seq-find #'file-readable-p
              (delq nil
                    (list (and input (expand-file-name
                                      name (file-name-directory input)))
                          (expand-file-name name wg21-latex-directory))))))

(defun wg21-latex--inline-inputs (file)
  "Return FILE's contents, with each \\input{NAME} it makes replaced by NAME.
NAME is read relative to FILE's directory, as TeX would from there,
and its own \\inputs are inlined too, so the result stands alone."
  (let ((dir (file-name-directory file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\\\input{\\([^}]+\\)}.*$" nil t)
        (let* ((name (match-string 1))
               (from (match-beginning 0))
               (to (match-end 0))
               (path (seq-find #'file-readable-p
                               (list (expand-file-name name dir)
                                     (expand-file-name (concat name ".tex") dir)))))
          (when path
            (let ((text (format "%%%% ---- %s\n%s" name (wg21-latex--inline-inputs path))))
              (delete-region from to)
              (goto-char from)
              (insert text)))))
      (buffer-string))))

(defun wg21-latex--prolog (info)
  "Return the paper's prolog, or nil for none.
INFO is a plist holding export options."
  (let ((name (org-trim (or (plist-get info :wg21-latex-prolog) ""))))
    (unless (member name '("" "none"))
      (let ((file (wg21-latex--find-file name info)))
        (unless file
          (user-error "LaTeX prolog %s not found" name))
        (concat "%% ---- " name "\n" (wg21-latex--inline-inputs file))))))

(defun wg21-latex-filter-options (info _backend)
  "Put the prolog at the head of the paper's #+LATEX_HEADER lines.
A header line that reads the prolog itself, as papers did with
\\include{common.tex} before it was the default, is dropped, since the
prolog is already there.  INFO is the export plist; return it."
  (let ((prolog (wg21-latex--prolog info))
        (name (file-name-sans-extension
               (org-trim (or (plist-get info :wg21-latex-prolog) "")))))
    (when prolog
      (plist-put info :latex-header
                 (concat prolog "\n"
                         (replace-regexp-in-string
                          (format "^[ \t]*\\\\\\(?:include\\|input\\){%s\\(?:\\.tex\\)?}[ \t]*\n?"
                                  (regexp-quote name))
                          "" (or (plist-get info :latex-header) ""))))))
  info)

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

;;; Title block

(defun wg21-latex--url (url)
  "Return URL protected for use in \\href."
  (replace-regexp-in-string "[%#\\\\]" "\\\\\\&" url))

(defcustom wg21-project "Programming Language C++"
  "The project a paper belongs to, set per paper by #+PROJECT."
  :group 'my-export-wg21
  :type 'string)

(defun wg21-latex--authors (info)
  "Return the paper's author lines, each with its email, as LaTeX.
Several authors, as in #+AUTHOR: A, B, pair with as many addresses in
#+EMAIL: a@x, b@y.  Otherwise the one author line carries every
address.  INFO is a plist holding export options."
  (let* ((text (lambda (value) (org-latex-plain-text value info)))
         (mail (lambda (address)
                 (format "{\\small\\textless\\href{mailto:%s}{%s}\\textgreater}"
                         (wg21-latex--url address) (funcall text address))))
         (author (org-trim (org-export-data (plist-get info :author) info)))
         (email (org-trim (org-export-data (plist-get info :email) info)))
         (names (split-string author "[ \t]*\\(?:,\\|\\band\\b\\)[ \t]*" t))
         (addresses (split-string email "[ \t,;]+" t)))
    (if (and (> (length names) 1) (= (length names) (length addresses)))
        (cl-mapcar (lambda (name address) (concat name " " (funcall mail address)))
                   names addresses)
      (list (mapconcat #'identity
                       (cons author (mapcar mail addresses))
                       " ")))))

(defun wg21-latex--title-block (info)
  "Return the title page's heading: title, authors, and document block.
The title and authors are centred, and the document block is set flush
right, as the working draft's papers set their title pages.  INFO is a
plist holding export options."
  (let* ((text (lambda (value) (org-latex-plain-text value info)))
         (git (wg21-git-metadata info))
         (repo (plist-get git :repo))
         (file (plist-get git :file))
         (url (plist-get git :url))
         (version (plist-get git :version))
         (row (lambda (label value)
                (format "\\wgmetalabel{%s} & \\wgmetavalue{%s} \\\\\n" label value))))
    (concat
     "\\begin{center}\n"
     (format "{\\LARGE %s\\par}\n\\medskip\n"
             (org-export-data (plist-get info :title) info))
     (mapconcat (lambda (line) (concat line "\\par\n")) (wg21-latex--authors info) "")
     "\\end{center}\n"
     "\\begin{flushright}\n\\begin{tabular}{@{}ll@{}}\n"
     (funcall row "Document \\#:" (org-export-data (plist-get info :docnumber) info))
     (funcall row "Date:" (org-export-data (org-export-get-date info) info))
     (funcall row "Project:" (org-export-data (plist-get info :project) info))
     (funcall row "Audience:" (org-export-data (plist-get info :audience) info))
     (when repo
       (funcall row "Source:" (format "\\href{%s}{%s}" (wg21-latex--url repo)
                                      (funcall text repo))))
     (when file
       (funcall row "" (if url
                           (format "\\href{%s}{%s}" (wg21-latex--url url) (funcall text file))
                         (funcall text file))))
     (when version
       (funcall row "" (format "\\texttt{%s}" (funcall text version))))
     "\\end{tabular}\n\\end{flushright}\n\\bigskip\n")))

(defun wg21-latex--link-citations (latex &optional bibliography)
  "Point each citation in LATEX at its reference's URL, when it has one.
A citation, \\cslcitation{N}{text}, links to entry N of the
bibliography; when \\cslbibitem{N}{...} holds exactly one URL, link
there instead.  The entries are read from BIBLIOGRAPHY, a text holding
them, by default LATEX itself.  See `wg21-cite-single-urls'."
  (let ((urls (wg21-cite-single-urls
               (mapcar (lambda (line)
                         (and (string-match "\\\\cslbibitem{\\([0-9]+\\)}" line)
                              (cons (match-string 1 line) (wg21-cite-urls line))))
                       (wg21-cite-matches "^\\\\cslbibitem{[0-9]+}.*$"
                                          (or bibliography latex))))))
    (replace-regexp-in-string
     "\\\\cslcitation{\\([0-9]+\\)}{"
     (lambda (citation)
       (save-match-data
         (string-match "{\\([0-9]+\\)}" citation)
         (let ((url (gethash (match-string 1 citation) urls)))
           (if url (format "\\href{%s}{" (wg21-latex--url url)) citation))))
     latex t t)))

(defun wg21-latex-footnote-reference (footnote-reference contents info)
  "Transcode FOOTNOTE-REFERENCE as \\wgfootnote, not \\footnote.
\\wgfootnote, from wg21org-preamble.tex, works whether the paper has
the \\footnote command or, from common.tex, a footnote environment.
CONTENTS is nil.  INFO is a plist holding export options."
  (replace-regexp-in-string
   "\\\\footnote{" "\\wgfootnote{"
   (org-latex-footnote-reference footnote-reference contents info)
   t t))

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
    (:project "PROJECT" nil wg21-project nil)
    (:source_repo "SOURCE_REPO" nil "" nil)
    (:source_file "SOURCE_FILE" nil "" parse)
    (:source_version "SOURCE_VERSION" nil "" parse)
    (:git_commit "GIT_COMMIT" nil "" parse)
    (:wg21-latex-preamble "WG21_LATEX_PREAMBLE" nil wg21-latex-preamble t)
    (:wg21-latex-prolog "WG21_LATEX_PROLOG" nil wg21-latex-prolog t)
    (:latex-class "LATEX_CLASS" nil wg21-latex-class t)
    (:latex-class-options "LATEX_CLASS_OPTIONS" nil wg21-latex-class-options t)
    ;; Only an address the paper gives, not the exporting user's.
    (:email "EMAIL" nil "" t)
    ;; Code set as in the editor; see `wg21-latex-engraved-theme'.
    (:latex-src-block-backend nil nil
     (if (featurep 'engrave-faces) 'engraved org-latex-src-block-backend))
    (:latex-engraved-theme "LATEX_ENGRAVED_THEME" nil wg21-latex-engraved-theme)
    (:wg21-toc-command nil nil wg21-toc-command))

  :translate-alist '((special-block . my-latex-special-block)
                     (headline . wg21-latex-headline)
                     (table . wg21-latex-table)
                     (src-block . wg21-latex-src-block)
                     (footnote-reference . wg21-latex-footnote-reference)
                     (template . my-wg21-latex-template))

  :filters-alist '((:filter-options . wg21-latex-filter-options)
                   (:filter-parse-tree . (wg21-seed-headline-references
                                          wg21-cite-drop-empty-bibliography)))

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
  (let* ((body (wg21-front-lift-abstract contents info))
         ;; Citations link through the bibliography in the whole body.
         (abstract (and (car body) (wg21-latex--link-citations (car body) contents)))
         (contents (wg21-latex--link-citations (cdr body) contents))
         (title (org-export-data (plist-get info :title) info))
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
     ;; The abstract, ahead of the table of contents; see wg21-front.el.
     (and abstract (concat abstract "\n\n"))
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
