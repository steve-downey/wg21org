;; ox-wg21html.el --- org exporter for WG21 papers in Latex format  -*- lexical-binding: t; -*-

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
(require 'url-util)
(require 'ox-html)
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

(defun my-html-special-block (special-block contents info)
  "Process my special block.  SPECIAL-BLOCK CONTENTS INFO.
Block names are case-insensitive in Org, but the class named after
one is not, so #+BEGIN_ABSTRACT gets the class abstract."
  (org-element-put-property special-block :type
                            (downcase (org-element-property :type special-block)))
  (let ((type (org-element-property :type special-block)))
    (cond
     ((string= type "cmptbl") (wg21-html-cmptbl special-block info))
     ((string= type "pnum")
      (format "<div class=\"pnum\">%s</div>\n" contents))
     ((member type '("codeblock" "itemdecl"))
      (wg21-html-raw-code-special-block special-block type info))
     (t (org-html-special-block special-block contents info)))))

(defun wg21-html-raw-code-special-block (block type info)
  "Export specgen BLOCK of TYPE as uninterpreted C++ code."
  (let ((code (org-html-encode-plain-text
               (wg21-special-block-raw-contents block))))
    (dolist (macro '("exposid" "exposidnc" "placeholder"))
      (setq code
            (replace-regexp-in-string
             (format "@\\\\%s{\\([^}]+\\)}@" macro)
             "<var>\\1</var>" code)))
    (setq code (replace-regexp-in-string "@\\\\seebelow@" "<var>see below</var>" code)
          code (replace-regexp-in-string "@\\\\impdef@" "<var>implementation-defined</var>" code)
          code (replace-regexp-in-string
                "\\\\ref{\\([^}]+\\)}"
                (lambda (match)
                  (let ((name (match-string 1 match)))
                    (format "<a href=\"%s\">[%s]</a>"
                            (wg21-stable-name-href name info) name)))
                code))
    (format "<div class=\"%s\"><pre class=\"src src-C++\">%s</pre></div>\n"
            type code)))

(defun wg21-html-table (table contents info)
  "Export TABLE, applying target-neutral WG21 column proportions."
  (let ((widths (wg21-table-columns table))
        (html (org-html-table table contents info)))
    (if (not widths)
        html
      (setq html
            (replace-regexp-in-string
             "<table"
             "<table class=\"wg21-spec-table\""
             html t t))
      (replace-regexp-in-string
       "<col\\([ 	][^>]*\\)>"
       (lambda (col)
         (if (not widths)
             col
           (prog1
               (format "<col style=\"width: %s%%\"%s>"
                       (car widths) (match-string 1 col))
             (setq widths (cdr widths)))))
       html t t))))

;;; Wording

(defun wg21-html-src-block (src-block contents info)
  "Transcode SRC-BLOCK, without syntax highlighting in wording.
In wording, edits to code are marked as the LaTeX export marks them,
with @\\added{...}@ and @\\removed{...}@, and become <ins> and <del>
here.  See wg21-wording.el.  CONTENTS is nil.  INFO is the export
plist."
  (if (wg21-wording-p src-block)
      (let ((org-html-htmlize-output-type nil))
        (replace-regexp-in-string
         "@\\\\\\(added\\|removed\\){\\([^}]*\\)}@"
         (lambda (edit)
           (save-match-data
             (string-match "\\\\\\(added\\|removed\\){\\([^}]*\\)}" edit)
             (let ((tag (if (string= (match-string 1 edit) "added") "ins" "del")))
               (format "<%s>%s</%s>" tag (match-string 2 edit) tag))))
         (org-html-src-block src-block contents info)
         t t))
    (org-html-src-block src-block contents info)))

;;; Comparison tables

;; Rows are grouped by wg21-cmptbl.el, shared with the LaTeX exporter.

(defun wg21-html--cmptbl-row (row tag columns info)
  "Return ROW as a <tr>, with cells as TAG elements.
COLUMNS is the width of the table.  INFO is the export plist."
  (if (not (wg21-cmptbl-cell-p (car row)))
      (let ((note (org-trim (org-export-data (car row) info))))
        (if (string-empty-p note) ""
          (format "<tr><td class=\"cmptbl-note\" colspan=\"%d\">%s</td></tr>\n"
                  columns note)))
    (concat
     "<tr>"
     (mapconcat
      (lambda (cell)
        (let ((side (wg21-cmptbl-side cell)))
          (format "<%s%s>%s</%s>"
                  tag
                  (if side (format " class=\"cmptbl-%s\"" side) "")
                  (org-trim (org-export-data (org-element-contents cell) info))
                  tag)))
      row "")
     ;; A row missing its after cell still spans the table.
     (let ((missing (- columns (length row))))
       (and (> missing 0)
            (format "<%s colspan=\"%d\"></%s>" tag missing tag)))
     "</tr>\n")))

(defun wg21-html-cmptbl (cmptbl info)
  "Transcode the comparison table CMPTBL into an HTML table.
INFO is a plist holding export options."
  (let* ((rows (wg21-cmptbl-rows cmptbl))
         (columns (apply #'max 1 (mapcar (lambda (row) (if (wg21-cmptbl-cell-p (car row)) (length row) 1))
                                         rows)))
         (head (and (wg21-cmptbl-header-p (car rows)) (pop rows)))
         (name (org-element-property :name cmptbl)))
    (concat
     (format "<table class=\"cmptbl\"%s>\n"
             (if name (format " id=\"%s\"" (org-html--reference cmptbl info)) ""))
     (and head
          (concat "<thead>\n" (wg21-html--cmptbl-row head "th" columns info) "</thead>\n"))
     "<tbody>\n"
     (mapconcat (lambda (row) (wg21-html--cmptbl-row row "td" columns info)) rows "")
     "</tbody>\n</table>\n")))


;; (defun my-wg21-export-to-html
;;     (&optional async subtreep visible-only body-only ext-plist)
;;   "Export current buffer."
;;   (interactive)
;;   (let ((file (org-export-output-file-name ".html" subtreep)))
;;     (org-export-to-file 'wg21-html file
;;       async subtreep visible-only body-only ext-plist)))


(defcustom wg21-document-number "Dnnnn"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-audience "WG21"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-project "Programming Language C++"
  "The project a paper belongs to, set per paper by #+PROJECT."
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-toc-div-id "toc"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

(defun wg21-html--mode-toggle ()
  "Return the metadata row that picks the page's light or dark theme.
System, the default, follows the reader's system.  The radio buttons
work with CSS alone, from wg21org.css, so the paper needs no script,
and so the choice lasts only while the page is open."
  (concat "<dt class=\"mode-toggle\">Theme:</dt><dd class=\"mode-toggle\">"
          (mapconcat
           (lambda (mode)
             (format "<label><input type=\"radio\" name=\"wg21-mode\" id=\"wg21-mode-%s\"%s> %s</label>"
                     (downcase mode)
                     (if (string= mode "System") " checked" "")
                     mode))
           '("Light" "Dark" "System") "")
          "</dd>\n"))

(defun wg21-html--diff-toggle (contents)
  "Return the metadata row that hides deleted text, if CONTENTS has any.
The checkbox works with CSS alone, from wg21org.css, so the paper needs
no script."
  (when (string-match-p "<del>\\|class=\"[^\"]*\\bremovedblock\\b" contents)
    (concat "<dt class=\"diff-toggle\">Wording:</dt>"
            "<dd class=\"diff-toggle\"><label>"
            "<input type=\"checkbox\" id=\"wg21-hide-deleted\"> "
            "Hide deleted text</label></dd>\n")))

(defun wg21-html-spec-metadata (contents info)
  "Return the document metadata block.
CONTENTS is the transcoded body.  INFO is a plist holding export options."
  (let* ((audience (plist-get info :audience))
         (docnumber (plist-get info :docnumber))
         (author (org-export-data (plist-get info :author) info))
         (date (plist-get info :date))
         (email (org-export-data (plist-get info :email) info))
         (git (wg21-git-metadata info))
         (enc #'org-html-encode-plain-text)
         (repo (plist-get git :repo))
         (file (plist-get git :file))
         (url (plist-get git :url))
         (version (plist-get git :version)))
    (concat
     "<div data-fill-with=\"spec-metadata\">\n<dl>\n"
     "<dt>Document #:</dt><dd>" (org-export-data docnumber info) "</dd>\n"
     "<dt>Date:</dt><dd>" (org-export-data date info) "</dd>\n"
     "<dt>Project:</dt><dd>" (org-export-data (plist-get info :project) info) "</dd>\n"
     "<dt>Audience:</dt><dd>" (org-export-data audience info) "</dd>\n"
     "<dt>Reply-to:</dt><dd>"
     (if (string-empty-p email)
         author
       (format "<a class=\"p-name fn u-email email\" href=\"mailto:%s\">%s &lt;%s&gt;</a>"
               email author email))
     "</dd>\n"
     (when (or repo file version)
       (concat
        "<dt>Source:</dt>"
        (when repo
          (format "<dd><a href=\"%s\">%s</a></dd>" (funcall enc repo) (funcall enc repo)))
        (when file
          (format "<dd>%s</dd>"
                  (if url
                      (format "<a href=\"%s\">%s</a>" (funcall enc url) (funcall enc file))
                    (funcall enc file))))
        (when version
          (format "<dd>%s</dd>" (funcall enc version)))
        "\n"))
     (wg21-html--mode-toggle)
     (wg21-html--diff-toggle contents)
     "</dl>\n</div>\n")))

(defcustom wg21-html-htmlize-output-type 'css
  "Value of `org-html-htmlize-output-type' for every WG21 HTML export.
The default, `css', emits a class per face (org-keyword,
org-rainbow-delimiters-depth-1, ...), so the code in a paper is
coloured by a face stylesheet such as modus-operandi-tinted.css or
modus-vivendi-tinted.css.  This overrides any setting in the paper."
  :group 'my-export-wg21
  :type '(choice (const css) (const inline-css) (const nil)))

(defun wg21-html-filter-options (info _backend)
  "Settings every WG21 export gets, whatever the paper says.
Code is coloured with face classes, see `wg21-html-htmlize-output-type'.
The export runs in a copy of the paper's buffer, so the buffer-local
value set here ends with the export.  The embedded stylesheets replace
Org's default style, and papers carry no scripts; an html-style or
html-scripts item in #+OPTIONS would otherwise turn them back on.
Return INFO."
  (setq-local org-html-htmlize-output-type wg21-html-htmlize-output-type)
  (plist-put info :html-head-include-default-style nil)
  (plist-put info :html-head-include-scripts nil)
  info)

;;; Face stylesheets

;; With `css' output, htmlize tags code with one class per face, and a
;; face stylesheet (modus-operandi-tinted.css, modus-vivendi-tinted.css)
;; colours them.  `org-html-htmlize-generate-css' writes that sheet
;; from the faces of the running Emacs, but in a form meant for pasting
;; into a page: wrapped in <style><!-- ... --></style>, with the theme's
;; colours on `body' and its link faces on `a'.  Linked as a .css file
;; the wrapper makes the browser drop the `body' rule, and the `a' rules
;; restyle every link on the page.  These functions turn it into a
;; stylesheet whose theme applies to code blocks only.

(defconst wg21-html-face-css-code-rule
  "pre.src code { color: inherit; background: transparent; }"
  "Rule letting a code block's theme colours show through its <code>.")

(defun wg21-html-face-css-fixup ()
  "Turn htmlize face CSS in the current buffer into a code-only stylesheet.
Safe to run on a buffer it has already fixed."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*\\(?:<style[^>]*>\\|<!--\\|-->\\|</style>\\)[ \t]*\n" nil t)
      (replace-match ""))
    (dolist (rule '(("body" . "pre.src")
                    ("a" . "pre.src a")
                    ("a:hover" . "pre.src a:hover")))
      (goto-char (point-min))
      (while (re-search-forward
              (format "^\\([ \t]*\\)%s {" (regexp-quote (car rule))) nil t)
        (replace-match (format "\\1%s {" (cdr rule)) t)))
    (goto-char (point-min))
    (unless (search-forward wg21-html-face-css-code-rule nil t)
      (goto-char (point-max))
      (insert "\n" wg21-html-face-css-code-rule "\n"))))

;;;###autoload
(defun wg21-html-write-face-css (file)
  "Write the faces of the running Emacs to FILE as a code stylesheet.
Run it in the editor with the theme loaded, so that exported code
looks the way it does there, rainbow delimiters included."
  (interactive "FWrite face stylesheet: ")
  (save-window-excursion
    (org-html-htmlize-generate-css)
    (unwind-protect
        (progn
          (wg21-html-face-css-fixup)
          (goto-char (point-min))
          (insert (format "/* Code faces from %s, written by `wg21-html-write-face-css' */\n"
                          (if custom-enabled-themes
                              (mapconcat #'symbol-name custom-enabled-themes ", ")
                            "the default theme")))
          (write-region (point-min) (point-max) file))
      (kill-buffer))))

;;;###autoload
(defun wg21-html-fix-face-css-file (file)
  "Fix the face stylesheet FILE in place; see `wg21-html-face-css-fixup'."
  (interactive "fFix face stylesheet: ")
  (with-temp-file file
    (insert-file-contents file)
    (wg21-html-face-css-fixup)))

;;; Math

;; LaTeX math is converted to MathML when the paper is exported, since
;; browsers render MathML themselves.  Org's default, MathJax, is a
;; script loaded from a CDN, which a self-contained paper cannot use.
;; A fragment that cannot be converted falls back to MathJax, and
;; `make check' then reports the paper.

(defcustom wg21-html-mathml-command
  '("pandoc" "--from=latex" "--to=html" "--mathml")
  "Command reading LaTeX math on stdin and writing HTML with MathML.
Set it to nil to leave math to Org, which uses MathJax."
  :group 'my-export-wg21
  :type '(choice (const :tag "Org default" nil) (repeat string)))

(defun wg21-html--mathml (latex)
  "Return LATEX as MathML, or nil if it cannot be converted."
  (when (and wg21-html-mathml-command
             (executable-find (car wg21-html-mathml-command)))
    (with-temp-buffer
      (insert latex)
      (when (eql 0 (apply #'call-process-region (point-min) (point-max)
                          (car wg21-html-mathml-command) t '(t nil) nil
                          (cdr wg21-html-mathml-command)))
        (let ((html (org-trim (buffer-string))))
          (and (string-match "<math[^>]*>\\(?:.\\|\n\\)*?</math>" html)
               (match-string 0 html)))))))

(defun wg21-html--math (element contents info fallback)
  "Transcode the LaTeX ELEMENT as MathML, or else with FALLBACK.
FALLBACK is Org's transcoder, called with ELEMENT, CONTENTS and INFO.
Using it is recorded in INFO, so the template adds MathJax."
  (let ((mathml (and (memq (plist-get info :with-latex) '(t mathjax))
                     (wg21-html--mathml (org-element-property :value element)))))
    (or mathml
        (progn (plist-put info :wg21-mathjax t)
               (funcall fallback element contents info)))))

(defun wg21-html-latex-fragment (fragment contents info)
  "Transcode a LaTeX FRAGMENT to MathML.  CONTENTS is nil.  INFO is the plist."
  (wg21-html--math fragment contents info #'org-html-latex-fragment))

(defun wg21-html-latex-environment (environment contents info)
  "Transcode a LaTeX ENVIRONMENT to MathML.  CONTENTS is nil.  INFO is the plist."
  (let ((mathml (wg21-html--math environment contents info
                                 #'org-html-latex-environment)))
    (if (string-prefix-p "<math" mathml)
        (format "<div class=\"equation-container\">%s</div>\n" mathml)
      mathml)))

;;; Embedded styles

;; A paper is uploaded as a single HTML file, so every stylesheet it
;; uses is copied into it.  Nothing is linked: a link to a file beside
;; the paper breaks once the paper is somewhere else, and a link to a
;; server breaks when the server changes.

(defconst wg21-html-directory
  (file-name-directory (or (macroexp-file-name) buffer-file-name))
  "The directory of this exporter, holding its default stylesheets.")

(defcustom wg21-html-style '("wg21org.css")
  "Stylesheets embedded in every paper, set per paper by #+WG21_STYLE.
A relative name is looked up next to the paper, then next to this
exporter."
  :group 'my-export-wg21
  :type '(repeat string))

(defcustom wg21-html-code-style '("modus-vivendi-tinted.css" "modus-operandi-tinted.css")
  "Face stylesheets for code, set per paper by #+WG21_CODE_STYLE.
Code is set opposite to the page, which sets it off from the text: the
first sheet colours code on a light page, and the second, if given,
on a dark one, whether the reader's system or the page's Theme switch
made it dark.  The second also colours printed code, since a printed
page is light and dark code costs ink.  With one sheet, code looks the
same in every mode.  Rules for faces the paper does not use are left
out.  Write a sheet with `wg21-html-write-face-css'."
  :group 'my-export-wg21
  :type '(repeat string))

(defun wg21-html--find-css (name info)
  "Return the file for stylesheet NAME, or nil if there is none.
INFO is a plist holding export options."
  (let ((input (plist-get info :input-file)))
    (seq-find #'file-readable-p
              (delq nil
                    (list (and input (expand-file-name
                                      name (file-name-directory input)))
                          (expand-file-name name wg21-html-directory))))))

(defun wg21-html--used-classes (html)
  "Return a hash table of the class names used in HTML."
  (let ((classes (make-hash-table :test #'equal))
        (start 0))
    (while (string-match "class=\"\\([^\"]*\\)\"" html start)
      (let ((names (match-string 1 html)))
        (setq start (match-end 0))
        (dolist (class (split-string names))
          (puthash class t classes))))
    classes))

(defun wg21-html--trim-css (css classes)
  "Drop the rules of CSS that only style classes missing from CLASSES.
Only a rule whose selectors are all single `.org-' classes, as in a
face stylesheet, can be dropped.  CSS with at-rules is returned
whole, since this does not parse nested blocks."
  (if (string-search "@" css)
      css
    (let ((start 0) kept)
      (while (string-match "\\([^{}]*\\){[^{}]*}" css start)
        (let* ((rule (match-string 0 css))
               (end (match-end 0))
               (selectors (split-string
                           (replace-regexp-in-string
                            "/\\*\\(?:[^*]\\|\\*+[^*/]\\)*\\*+/" ""
                            (match-string 1 css))
                           "," t "[ \t\n]+"))
               (unused (seq-every-p
                        (lambda (selector)
                          (and (string-match "\\`\\.\\(org-[[:alnum:]_-]+\\)\\'" selector)
                               (not (gethash (match-string 1 selector) classes))))
                        selectors)))
          (unless unused (push (org-trim rule) kept))
          (setq start end)))
      (mapconcat #'identity (nreverse kept) "\n"))))

(defun wg21-html--scope-css (css scope)
  "Return the flat stylesheet CSS with SCOPE put before every selector.
So `.org-keyword' becomes `SCOPE .org-keyword': the rule applies only
within what SCOPE matches, and outranks the same rule unscoped."
  (let ((start 0) rules)
    (while (string-match "\\([^{}]*\\)\\({[^{}]*}\\)" css start)
      (let* ((end (match-end 0))
             (block (match-string 2 css))
             (selectors (split-string
                        (replace-regexp-in-string
                         "/\\*\\(?:[^*]\\|\\*+[^*/]\\)*\\*+/" "" (match-string 1 css))
                        "," t "[ \t\n]+")))
        (setq start end)
        (when selectors
          (push (concat (mapconcat (lambda (selector) (concat scope " " selector))
                                   selectors ", ")
                        " " block)
                rules))))
    (mapconcat #'identity (nreverse rules) "\n")))

(defun wg21-html--dark-page-css (css)
  "Return CSS set to apply where the page is dark, and in print.
The page is dark on a dark system unless the reader picked Light in
the Theme switch, or wherever the reader picked Dark; see wg21org.css."
  (concat "@media print {\n" css "\n}\n"
          "@media screen and (prefers-color-scheme: dark) {\n"
          (wg21-html--scope-css css ":root:not(:has(#wg21-mode-light:checked))")
          "\n}\n"
          "@media screen {\n"
          (wg21-html--scope-css css ":root:has(#wg21-mode-dark:checked)")
          "\n}"))

(defun wg21-html--style-element (file classes &optional wrap)
  "Return a <style> element holding the stylesheet FILE.
Rules for classes missing from CLASSES are dropped, see
`wg21-html--trim-css'.  WRAP, if given, is a function applied to the
CSS that is left, to limit where it applies."
  (let ((css (wg21-html--trim-css
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))
              classes)))
    (format "<style>\n/* %s */\n%s\n</style>\n"
            (file-name-nondirectory file)
            (if wrap (funcall wrap css) css))))

(defun wg21-html--embedded-styles (contents info)
  "Return the <style> elements for the paper, and the files they hold.
The result is (HTML . FILES), where FILES are the sheets that apply
on every medium.  CONTENTS is the transcoded body, used
to leave out unused code faces.  INFO is a plist holding export
options."
  (let ((classes (wg21-html--used-classes contents))
        files html)
    (cl-flet ((embed (name &optional wrap)
                (let ((file (wg21-html--find-css name info)))
                  (if (not file)
                      (user-error "Stylesheet %s not found" name)
                    ;; Only a sheet that always applies stands in for a link.
                    (unless wrap (push (file-truename file) files))
                    (push (wg21-html--style-element file classes wrap) html)))))
      (mapc #'embed (plist-get info :wg21-style))
      (let ((code (plist-get info :wg21-code-style)))
        (when code (embed (car code)))
        (when (cadr code) (embed (cadr code) #'wg21-html--dark-page-css))))
    (cons (apply #'concat (nreverse html)) files)))

(defun wg21-html--inline-stylesheet-links (head contents info embedded)
  "Replace links to local stylesheets in HEAD with their contents.
A stylesheet already in EMBEDDED, a list of files, is dropped rather
than copied twice.  Links to other servers are left for `make check'
to report.  CONTENTS is the transcoded body.  INFO is a plist holding
export options."
  (let ((classes (wg21-html--used-classes contents)))
    (replace-regexp-in-string
     "<link[^>]*rel=[\"']stylesheet[\"'][^>]*>\n?"
     (lambda (link)
       (save-match-data
       (let* ((href (and (string-match "href=[\"']\\([^\"']*\\)[\"']" link)
                         (match-string 1 link)))
              (file (and href
                         (not (string-match-p "\\`\\(?:[a-z]+:\\)?//" href))
                         (wg21-html--find-css href info))))
         (cond
          ((not file) link)
          ((member (file-truename file) embedded) "")
          (t (wg21-html--style-element file classes))))))
     head t t)))

;;; Embedded images

(defconst wg21-html-image-types
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("svg" . "image/svg+xml") ("webp" . "image/webp")
    ("bmp" . "image/bmp"))
  "Media types of the images embedded in a paper, by file extension.")

(defun wg21-html--data-uri (file)
  "Return FILE as a data: URI, or nil if it is not an image type we know."
  (let ((type (cdr (assoc (downcase (or (file-name-extension file) ""))
                          wg21-html-image-types))))
    (when type
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (base64-encode-region (point-min) (point-max) t)
        (concat "data:" type ";base64," (buffer-string))))))

(defun wg21-html--unescape (text)
  "Undo the HTML escaping Org applies to an attribute value TEXT."
  (replace-regexp-in-string
   "&\\(amp\\|lt\\|gt\\|quot\\|#39\\);"
   (lambda (entity)
     (cdr (assoc entity '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                          ("&quot;" . "\"") ("&#39;" . "'")))))
   text t t))

(defun wg21-html--embed-images (html info)
  "Replace each local image HTML loads with the image itself, as a data: URI.
An image elsewhere, or a local file that cannot be read, is left as it
is, with a message, for `make check' to report.  A relative name is
looked up next to the paper.  INFO is a plist holding export options."
  (let* ((input (plist-get info :input-file))
         (dir (if input (file-name-directory input) default-directory)))
    (replace-regexp-in-string
     "<img\\b[^>]*?\\bsrc=\"\\([^\"]*\\)\""
     (lambda (tag)
       (save-match-data
         (string-match "\\bsrc=\"\\([^\"]*\\)\"" tag)
         (let* ((src (match-string 1 tag))
                (start (match-beginning 1))
                (end (match-end 1))
                (local (cond ((string-prefix-p "file://" src)
                              (substring src (length "file://")))
                             ((not (string-match-p "\\`[a-z][a-z0-9+.-]*:" src))
                              src)))
                (path (and local
                           (expand-file-name
                            (url-unhex-string (wg21-html--unescape local))
                            dir)))
                (uri (and path (file-readable-p path) (wg21-html--data-uri path))))
           (cond
            (uri (concat (substring tag 0 start) uri (substring tag end)))
            (t (when (and path (not (string-prefix-p "data:" src)))
                 (message "wg21-html: cannot embed image %s" src))
               tag)))))
     html t t)))

;;; Citations

(defun wg21-html--link-citations (html)
  "Point each citation in HTML at its reference's URL, when it has one.
A citation links to its entry in the bibliography; when that entry
holds exactly one URL, link there instead, with the whole entry as
the link's title, so a reader goes straight to the cited paper and
can still see the reference by hovering.  See `wg21-cite-single-urls'."
  (let ((entries nil)
        (titles (make-hash-table :test #'equal))
        (start 0))
    (while (string-match
            "<div class=\"csl-entry\"><a id=\"citeproc_bib_item_\\([0-9]+\\)\"></a>\\(\\(?:.\\|\n\\)*?\\)</div>"
            html start)
      (let ((item (match-string 1 html))
            (entry (match-string 2 html)))
        (setq start (match-end 0))
        (push (cons item (append (wg21-cite-matches "href=\"\\(https?://[^\"]*\\)\"" entry 1)
                                 (wg21-cite-urls (replace-regexp-in-string "<[^>]*>" " " entry))))
              entries)
        (puthash item
                 (string-trim
                  (replace-regexp-in-string
                   "[ \t\n]+" " "
                   (replace-regexp-in-string "\"" "&quot;"
                                             (replace-regexp-in-string "<[^>]*>" "" entry))))
                 titles)))
    (let ((urls (wg21-cite-single-urls entries)))
      (replace-regexp-in-string
       "<a href=\"#citeproc_bib_item_\\([0-9]+\\)\">"
       (lambda (link)
         (save-match-data
           (string-match "citeproc_bib_item_\\([0-9]+\\)" link)
           (let* ((item (match-string 1 link))
                  (url (gethash item urls)))
             (if url
                 (format "<a href=\"%s\" title=\"%s\">" url (gethash item titles))
               link))))
       html t t))))

(defun wg21-html--head (contents info)
  "Return the <head> contents after the meta information.
CONTENTS is the transcoded body.  INFO is a plist holding export options."
  (let ((embedded (wg21-html--embedded-styles contents info)))
    (concat (car embedded)
            (wg21-html--inline-stylesheet-links
             (org-html--build-head info) contents info (cdr embedded)))))

(defun wg21-html--meta-info (info)
  "Return `org-html--build-meta-info' with a plain text <title>.
Org puts the title's markup, such as ~code~, into <title> as is.
INFO is a plist holding export options."
  (let ((title (org-trim
                (replace-regexp-in-string
                 "<[^>]*>" ""
                 (org-export-data (plist-get info :title) info)))))
    (replace-regexp-in-string "<title>.*?</title>"
                              (format "<title>%s</title>" title)
                              (org-html--build-meta-info info)
                              t t)))

(org-export-define-derived-backend 'wg21-html 'html
  :options-alist
  '((:docnumber "DOCNUMBER" nil wg21-document-number nil)
    (:source_repo "SOURCE_REPO" nil "" nil)
    (:source_file "SOURCE_FILE" nil "" parse)
    (:source_version "SOURCE_VERSION" nil "" parse)
    (:git_commit "GIT_COMMIT" nil "" parse)
    (:audience "AUDIENCE" nil wg21-audience nil)
    (:project "PROJECT" nil wg21-project nil)
    (:toc-div-id "TOC_DIV_ID" nil wg21-toc-div-id nil)
    (:wg21-style "WG21_STYLE" nil wg21-html-style split)
    (:wg21-code-style "WG21_CODE_STYLE" nil wg21-html-code-style split)
    ;; Only an address the paper gives, not the exporting user's.
    (:email "EMAIL" nil "" t)
    (:html-self-link-headlines nil nil t)
    (:html-wrap-src-lines nil nil org-html-wrap-src-lines))

  :translate-alist '((special-block . my-html-special-block)
                     (table . wg21-html-table)
                     (src-block . wg21-html-src-block)
                     (latex-fragment . wg21-html-latex-fragment)
                     (latex-environment . wg21-html-latex-environment)
                     (inner-template . my-wg21-html-inner-template)
                     (headline . my-wg21-html-headline)
                     (keyword . my-wg21-html-keyword)
                     (template . my-wg21-html-template))

  :filters-alist '((:filter-options . wg21-html-filter-options)
                   (:filter-parse-tree . (wg21-seed-headline-references
                                          wg21-cite-drop-empty-bibliography)))

  :menu-entry '(?w "Export WG21 Paper"
                   ((?H "As HTML buffer" my-wg21-export-as-html)
	                (?h "As HTML file" my-wg21-export-to-html)
	                (?o "As HTML file and open"
	                    (lambda (a s v b)
	                      (if a (my-wg21-export-to-html t s v b)
		                    (org-open-file (my-wg21-export-to-html nil s v b))))))))


(defun my-wg21-html-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((body (wg21-front-lift-abstract contents info)))
    (concat
     ;; The abstract, ahead of the table of contents; see wg21-front.el.
     (and (car body) (concat (car body) "\n"))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth (my-wg21-html-toc depth info)))
     ;; Document contents.
     (cdr body)
     ;; Footnotes section.
     (org-html-footnote-section info))))

(defun my-wg21-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (setq contents (wg21-html--link-citations
                  (wg21-html--embed-images contents info)))
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let ((decl (or (and (stringp org-html-xml-declaration)
			              org-html-xml-declaration)
			         (cdr (assoc (plist-get info :html-extension)
				                 org-html-xml-declaration))
			         (cdr (assoc "html" org-html-xml-declaration))

			         "")))
       (when (not (or (eq nil decl) (string= "" decl)))
	     (format "%s\n"
		         (format decl
		                 (or (and org-html-coding-system
			                      (fboundp 'coding-system-get)
			                      (coding-system-get org-html-coding-system 'mime-charset))
		                     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	       (when (org-html-xhtml-p info)
	         (format
	          " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
	          (plist-get info :language) (plist-get info :language)))
	       ">\n")
   "<head>\n"
   (wg21-html--meta-info info)
   (wg21-html--head contents info)
   (when (plist-get info :wg21-mathjax)
     (org-html--build-mathjax-config info))
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	     (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format org-html-home/up-format
	           (or link-up link-home)
	           (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (format "<%s id=\"%s\">\n"
	       (nth 1 (assq 'content org-html-divs))
	       (nth 2 (assq 'content org-html-divs)))
   ;; Document title.
   (let ((title (plist-get info :title)))
     (format "<h1 class=\"title\">%s</h1>\n" (org-export-data (or title "") info)))
   ;; DOCBLOCK
   (wg21-html-spec-metadata contents info)
   contents
   (format "</%s>\n"
	       (nth 1 (assq 'content org-html-divs)))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Closing document.
   "</body>\n</html>"))




;;; Tables of Contents

(defun org-html-format-headline-default-function
    (todo _todo-type priority text tags info)
  "Default format function for a headline.
See `org-html-format-headline-function' for details and the
description of TODO, PRIORITY, TEXT, TAGS, and INFO arguments."
  (let ((todo (org-html--todo todo info))
	    (priority (org-html--priority priority info))
	    (tags (org-html--tags tags info)))
    (concat todo (and todo " ")
	        priority (and priority " ")
	        text
	        (and tags "&#xa0;&#xa0;&#xa0;") tags)))


;;;<a href="#example-hello-world"><span class="secno">1.3.1</span> <span class="content">Hello world</span></a>
(defun my-wg21-html--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
	     (todo (and (plist-get info :with-todo-keywords)
		            (let ((todo (org-element-property :todo-keyword headline)))
		              (and todo (org-export-data todo info)))))
	     (todo-type (and todo (org-element-property :todo-type headline)))
	     (priority (and (plist-get info :with-priority)
			            (org-element-property :priority headline)))
	     (text (org-export-data-with-backend
		        (org-export-get-alt-title headline info)
		        (org-export-toc-entry-backend 'html)
		        info))
	     (tags (and (eq (plist-get info :with-tags) t)
		            (org-export-get-tags headline info))))
    (format "<a href=\"#%s\"><span class=\"secno\">%s</span> <span class=\"content\">%s</span></a>"
	        ;; Label.
	        (org-html--reference headline info)
	        ;; Number.
	        (and (not (org-export-low-level-p headline info))
		         (org-export-numbered-headline-p headline info)
		         (concat (mapconcat #'number-to-string headline-number ".")
			             " "))
            ;; Content
	        text)))

(defun my-wg21-html--toc-text (toc-entries)
  "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
  (let* ((prev-level (1- (cdar toc-entries)))
	     (start-level prev-level))
    (concat
     (mapconcat
      (lambda (entry)
	    (let ((headline (car entry))
	          (level (cdr entry)))
	      (concat
	       (let* ((cnt (- level prev-level))
		          (times (if (> cnt 0) (1- cnt) (- cnt))))
	         (setq prev-level level)
	         (concat
	          (org-html--make-string
	           times (cond ((> cnt 0) "\n<ul class=\"toc\">\n<li>")
			               ((< cnt 0) "</li>\n</ul>\n")))
	          (if (> cnt 0) "\n<ul class=\"toc\">\n<li>" "</li>\n<li>")))
	       headline)))
      toc-entries "")
     (org-html--make-string (- prev-level start-level) "</li>\n</ul>\n"))))

(defun my-wg21-html-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
	     (mapcar (lambda (headline)
		           (cons (my-wg21-html--format-toc-headline headline info)
			             (org-export-get-relative-level headline info)))
		         (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let ((toc (concat ;; "<div id=\"toc\" role=\"doc-toc\">"
			      (my-wg21-html--toc-text toc-entries)
			      ;; "</div>\n"
                  "\n"
                  )))
	    (if scope toc
	      (let ((outer-tag (if (org-html--html5-fancy-p info)
			                   "nav"
			                 "div"))
                (toc-div-id (plist-get info :toc-div-id)))
	        (concat (format "<%s id=\"%s\" role=\"doc-toc\">\n" outer-tag (org-export-data toc-div-id info))
		            (let ((top-level (plist-get info :html-toplevel-hlevel)))
		              (format "<h%d class=\"no-num no-toc no-ref\" id=\"contents\">%s</h%d>\n"
			                  top-level
			                  (org-html--translate "Table of Contents" info)
			                  top-level))
		            toc
		            (format "</%s>\n" outer-tag))))))))

;;;; Headline

(defun my-wg21-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
	       (id (org-html--reference headline info))
           (source (wg21-git-headline-url headline info))
	       (formatted-text
            (concat
             (format "<span class=\"content\">%s</span>" full-text)
             (when (plist-get info :html-self-link-headlines)
               (format "<a class=\"self-link\" href=\"#%s\" aria-label=\"Link to this section\"></a>" id))
             (when source
               (format "<a class=\"source-link\" href=\"%s\" title=\"This section in the Org source\">source</a>"
                       (org-html-encode-plain-text source))))))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
	        (concat
	         (and (org-export-first-sibling-p headline info)
		          (apply #'format "<%s class=\"org-%s\">\n"
			             (make-list 2 html-type)))
	         (org-html-format-list-item
	          contents (if numberedp 'ordered 'unordered)
	          nil info nil
	          (concat (org-html--anchor id nil nil info) formatted-text)) "\n"
	         (and (org-export-last-sibling-p headline info)
		          (format "</%s>\n" html-type))))
	    ;; Standard headline.  Export it as a section.
        (let ((extra-class
	           (org-element-property :HTML_CONTAINER_CLASS headline))
	          (headline-class
	           (org-element-property :HTML_HEADLINE_CLASS headline))
              (first-content (car (org-element-contents headline))))
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (format "outline-container-%s" id)
                  (string-join
                   (delq nil (list (format "outline-%d" level)
                                   extra-class
                                   (and (org-element-property :WG21_WORDING headline)
                                        "wg21-wording")))
                   " ")
                  (format "\n<h%d class=\"heading%s\" id=\"%s\">%s</h%d>\n"
                          level
                          (if headline-class (concat " " headline-class) "")
                          id
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (concat (mapconcat #'number-to-string numbers ".") ".")))
                           formatted-text)
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

(defun my-wg21-html-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	    (value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value)
     ((string= key "TOC")
      (let ((case-fold-search t))
	    (cond
	     ((string-match "\\<headlines\\>" value)
	      (let ((depth (and (string-match "\\<[0-9]+\\>" value)
			                (string-to-number (match-string 0 value))))
		        (scope
		         (cond
		          ((string-match ":target +\\(\".+?\"\\|\\S-+\\)" value) ;link
		           (org-export-resolve-link
		            (org-strip-quotes (match-string 1 value)) info))
		          ((string-match-p "\\<local\\>" value) keyword)))) ;local
	        (my-wg21-html-toc depth info scope)))
	     ((string= "listings" value) (org-html-list-of-listings info))
	     ((string= "tables" value) (org-html-list-of-tables info))))))))



;;; End-user functions

;;;###autoload
(defun my-wg21-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

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
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'wg21-html "*WG21 HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun my-wg21-convert-region-to-html ()
  "Assume the current region has Org syntax, and convert it to HTML.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an HTML buffer and use this command
to convert it."
  (interactive)
  (org-export-replace-region-by 'wg21-html))

(defalias 'my-wg21-export-region-to-html #'my-wg21-convert-region-to-html)

;;;###autoload
(defun my-wg21-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

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
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat
		             (when (> (length org-html-extension) 0) ".")
		             (or (plist-get ext-plist :html-extension)
			             org-html-extension
			             "html")))
	     (file (org-export-output-file-name extension subtreep))
	     (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'wg21-html file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun my-wg21-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'wg21-html filename
		              (concat (when (> (length org-html-extension) 0) ".")
			                  (or (plist-get plist :html-extension)
				                  org-html-extension
				                  "html"))
		              plist pub-dir))


(provide 'ox-wg21html)
;;; ox-wg21html.el ends here
