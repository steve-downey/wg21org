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
(require 'ox-html)
(require 'format-spec)
(require 'wg21-links
         (expand-file-name "wg21-links"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

(defun my-html-special-block (special-block contents info)
  "Process my special block.  SPECIAL-BLOCK CONTENTS INFO."
  (if (string= (downcase (org-element-property :type special-block)) "cmptbl")
      (wg21-html-cmptbl special-block info)
    (org-html-special-block special-block contents info)))

;;; Comparison tables

;; #+begin_cmptbl holds #+begin_cmptblcell blocks whose parameter names
;; their column, `before' or `after'.  Each `before' starts a row.  A
;; first row with no code in it is the header.  The LaTeX exporter reads
;; the same markup.

(defun wg21-html--cmptbl-cell-p (element)
  "Non-nil if ELEMENT is a cmptblcell special block."
  (and (eq (org-element-type element) 'special-block)
       (string= (downcase (org-element-property :type element)) "cmptblcell")))

(defun wg21-html--cmptbl-side (cell)
  "Return the column name of CELL, from its parameter, or nil."
  (let ((side (org-element-property :parameters cell)))
    (and side (downcase (org-trim side)))))

(defun wg21-html--cmptbl-rows (cmptbl)
  "Group the children of CMPTBL into rows.
Each row is a list of cells.  Anything that is not a cell is a row of
its own, a list of just that element, so it is shown rather than dropped."
  (let (rows row)
    (dolist (child (org-element-contents cmptbl))
      (cond
       ((not (wg21-html--cmptbl-cell-p child))
        (when row (push (nreverse row) rows) (setq row nil))
        (push (list child) rows))
       ((or (equal (wg21-html--cmptbl-side child) "before")
            (>= (length row) 2))
        (when row (push (nreverse row) rows))
        (setq row (list child)))
       (t (push child row))))
    (when row (push (nreverse row) rows))
    (nreverse rows)))

(defun wg21-html--cmptbl-header-p (row)
  "Non-nil if ROW, a list of cells, holds only prose, so is a header."
  (and (consp row)
       (seq-every-p #'wg21-html--cmptbl-cell-p row)
       (not (org-element-map row '(src-block example-block fixed-width table)
              #'identity nil t))))

(defun wg21-html--cmptbl-row (row tag columns info)
  "Return ROW as a <tr>, with cells as TAG elements.
COLUMNS is the width of the table.  INFO is the export plist."
  (if (not (wg21-html--cmptbl-cell-p (car row)))
      (let ((note (org-trim (org-export-data (car row) info))))
        (if (string-empty-p note) ""
          (format "<tr><td class=\"cmptbl-note\" colspan=\"%d\">%s</td></tr>\n"
                  columns note)))
    (concat
     "<tr>"
     (mapconcat
      (lambda (cell)
        (let ((side (wg21-html--cmptbl-side cell)))
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
  (let* ((rows (wg21-html--cmptbl-rows cmptbl))
         (columns (apply #'max 1 (mapcar (lambda (row) (if (wg21-html--cmptbl-cell-p (car row)) (length row) 1))
                                         rows)))
         (head (and (wg21-html--cmptbl-header-p (car rows)) (pop rows)))
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

(defcustom wg21-toc-div-id "toc"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

;;; Git metadata

(defcustom wg21-git-remote "origin"
  "Git remote whose URL is used when #+SOURCE_REPO is not given."
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-forge-blob-url-formats
  '(("\\`https?://github\\.com/" "%r/blob/%c/%p" "?plain=1#L%l")
    ("\\`https?://gitlab\\.com/" "%r/-/blob/%c/%p" "?plain=1#L%l")
    ("" "%r/src/commit/%c/%p" "?display=source#L%l"))
  "Permalink formats for a file at a commit, per forge.
Each entry is (REGEXP FILE-FORMAT LINE-FORMAT); the first REGEXP
matching the repository URL wins.  In FILE-FORMAT, %r is the
repository URL, %c the commit, and %p the path of the file within
the repository.  LINE-FORMAT is appended to link to line %l of the
file's source, rather than to a rendered view where lines have no
anchors.  The catch-all entry is the Gitea/Forgejo layout."
  :group 'my-export-wg21
  :type '(repeat (list regexp string string)))

(defun wg21-git-output (&rest args)
  "Run git with ARGS in `default-directory'; return its output.
Return nil if git fails or prints nothing.  Nil ARGS are dropped, so
a missing `buffer-file-name' does not become a literal argument."
  (with-temp-buffer
    (when (and (eql 0 (apply #'process-file "git" nil '(t nil) nil
                             (delq nil args)))
               (> (buffer-size) 0))
      (buffer-string))))

(defun wg21-git-string (&rest args)
  "Run git with ARGS in `default-directory'; return its first output line.
Return nil if git fails or prints nothing."
  (let ((output (apply #'wg21-git-output args)))
    (and output (car (split-string output "\n")))))

(defun wg21-git-https-url (url)
  "Turn the git remote URL into the https URL of its web page.
Handles scp-style (git@host:owner/repo), ssh://, and http(s) remotes.
An ssh port is dropped, since it says nothing about the web port."
  (when url
    (let ((url (string-remove-suffix ".git" (string-remove-suffix "/" url))))
      (cond
       ((string-match "\\`https?://" url) url)
       ((string-match "\\`ssh://\\(?:[^@/]+@\\)?\\([^:/]+\\)\\(?::[0-9]+\\)?/\\(.*\\)\\'" url)
        (format "https://%s/%s" (match-string 1 url) (match-string 2 url)))
       ((string-match "\\`\\(?:[^@/]+@\\)?\\([^:/]+\\):\\(.*\\)\\'" url)
        (format "https://%s/%s" (match-string 1 url) (match-string 2 url)))))))

(defun wg21-git-blob-url (repo commit path &optional line)
  "Return a permalink to PATH at COMMIT in the web view of REPO.
With LINE, link to that line of the file's source.
See `wg21-forge-blob-url-formats'."
  (when (and repo commit path)
    (let ((formats (cdr (seq-find (lambda (entry) (string-match-p (car entry) repo))
                                  wg21-forge-blob-url-formats)))
          (spec `((?r . ,repo) (?c . ,commit) (?p . ,path) (?l . ,line))))
      (concat (format-spec (nth 0 formats) spec)
              (and line (format-spec (nth 1 formats) spec))))))

(defun wg21-source-url (&optional file)
  "Return a permalink to FILE, by default the current buffer's file, at HEAD.
Intended for use in macros, e.g.
  #+MACRO: permalink (eval (wg21-source-url))"
  (let ((file (or file (buffer-file-name))))
    (when file
      (let ((default-directory (file-name-directory file)))
        (wg21-git-blob-url
         (wg21-git-https-url
          (wg21-git-string "remote" "get-url" wg21-git-remote))
         (wg21-git-string "rev-parse" "HEAD")
         (wg21-git-string "ls-files" "--full-name" "--" file))))))

(defun wg21-git-metadata (info)
  "Return a plist of git metadata for the document being exported.
The result is computed once per export and kept in INFO."
  (or (plist-get info :wg21-git)
      (plist-get (plist-put info :wg21-git (wg21-git--metadata info)) :wg21-git)))

(defun wg21-git--metadata (info)
  "Return a plist of git metadata for the document being exported.
Each of :repo, :file, :version and :commit comes from the matching
keyword (SOURCE_REPO, SOURCE_FILE, SOURCE_VERSION, GIT_COMMIT) if the
document sets it, and otherwise from git.  :url is a permalink to the
file at the commit.  Everything is nil outside a git work tree.
INFO is a plist holding export options."
  (let* ((input (plist-get info :input-file))
         (default-directory (if input (file-name-directory input)
                              default-directory))
         (keyword (lambda (prop)
                    (let ((value (org-trim
                                  (org-element-interpret-data
                                   (plist-get info prop)))))
                      (and (not (string-empty-p value)) value))))
         (repo (or (funcall keyword :source_repo)
                   (wg21-git-https-url
                    (wg21-git-string "remote" "get-url" wg21-git-remote))))
         (file (or (funcall keyword :source_file)
                   (and input (wg21-git-string "ls-files" "--full-name"
                                               "--" input))))
         (version (or (funcall keyword :source_version)
                      (wg21-git-string "describe" "--always" "--long" "--all"
                                       "--dirty" "--tags")))
         (commit (or (funcall keyword :git_commit)
                     (wg21-git-string "rev-parse" "HEAD"))))
    (list :repo repo :file file :version version :commit commit
          :url (and file (wg21-git-blob-url repo commit file)))))

(defun wg21-git--headline-lines (info)
  "Map each headline to its line in the committed source of the document.
Return a hash table from headline element to line number.  Headlines
are found in document order in `git show COMMIT:FILE', so the links
point at the file the permalinks name, not at unsaved edits.  A
headline with no match there, such as a new one or one from
#+INCLUDE, is absent.  INFO is a plist holding export options."
  (let* ((git (wg21-git-metadata info))
         (lines (make-hash-table :test #'eq))
         (input (plist-get info :input-file))
         (default-directory (if input (file-name-directory input)
                              default-directory))
         (text (and (plist-get git :commit) (plist-get git :file)
                    (wg21-git-output "show" (concat (plist-get git :commit) ":"
                                                    (plist-get git :file))))))
    (when text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (org-element-map (plist-get info :parse-tree) 'headline
          (lambda (headline)
            (let ((start (point))
                  (title (org-element-property :raw-value headline)))
              (if (re-search-forward (concat "^\\*+ .*" (regexp-quote title)) nil t)
                  (puthash headline (line-number-at-pos) lines)
                (goto-char start))))
          info)))
    lines))

(defun wg21-git-headline-url (headline info)
  "Return a permalink to HEADLINE's line in the document's source, or nil.
INFO is a plist holding export options."
  (let* ((lines (or (plist-get info :wg21-headline-lines)
                    (plist-get (plist-put info :wg21-headline-lines
                                          (wg21-git--headline-lines info))
                               :wg21-headline-lines)))
         (line (gethash headline lines))
         (git (wg21-git-metadata info)))
    (and line
         (wg21-git-blob-url (plist-get git :repo) (plist-get git :commit)
                            (plist-get git :file) line))))

(defun wg21-html-spec-metadata (_contents info)
  "Return the document metadata block.
INFO is a plist holding export options."
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
          (and (string-match "<math[^>]*>.*</math>" html)
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

(defcustom wg21-html-code-style '("modus-operandi-tinted.css" "modus-vivendi-tinted.css")
  "Face stylesheets for code, set per paper by #+WG21_CODE_STYLE.
The first colours code on screen and in print; the second, if given,
replaces it on screens in dark mode.  Rules for faces the paper does
not use are left out.  Write one with `wg21-html-write-face-css'."
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

(defun wg21-html--style-element (file classes &optional media)
  "Return a <style> element holding the stylesheet FILE.
Rules for classes missing from CLASSES are dropped, see
`wg21-html--trim-css'.  With MEDIA, the rules apply only under that
media query."
  (let ((css (wg21-html--trim-css
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))
              classes)))
    (format "<style>\n/* %s */\n%s\n</style>\n"
            (file-name-nondirectory file)
            (if media (format "@media %s {\n%s\n}" media css) css))))

(defun wg21-html--embedded-styles (contents info)
  "Return the <style> elements for the paper, and the files they hold.
The result is (HTML . FILES), where FILES are the sheets that apply
on every medium.  CONTENTS is the transcoded body, used
to leave out unused code faces.  INFO is a plist holding export
options."
  (let ((classes (wg21-html--used-classes contents))
        files html)
    (cl-flet ((embed (name &optional media)
                (let ((file (wg21-html--find-css name info)))
                  (if (not file)
                      (user-error "Stylesheet %s not found" name)
                    ;; Only a sheet that always applies stands in for a link.
                    (unless media (push (file-truename file) files))
                    (push (wg21-html--style-element file classes media) html)))))
      (mapc #'embed (plist-get info :wg21-style))
      (let ((code (plist-get info :wg21-code-style)))
        (when code (embed (car code)))
        (when (cadr code) (embed (cadr code) "screen and (prefers-color-scheme: dark)"))))
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
    (:toc-div-id "TOC_DIV_ID" nil wg21-toc-div-id nil)
    (:wg21-style "WG21_STYLE" nil wg21-html-style split)
    (:wg21-code-style "WG21_CODE_STYLE" nil wg21-html-code-style split)
    ;; Only an address the paper gives, not the exporting user's.
    (:email "EMAIL" nil "" t)
    (:html-self-link-headlines nil nil t)
    (:html-wrap-src-lines nil nil org-html-wrap-src-lines))

  :translate-alist '((special-block . my-html-special-block)
                     (latex-fragment . wg21-html-latex-fragment)
                     (latex-environment . wg21-html-latex-environment)
                     (inner-template . my-wg21-html-inner-template)
                     (headline . my-wg21-html-headline)
                     (keyword . my-wg21-html-keyword)
                     (template . my-wg21-html-template))

  :filters-alist '((:filter-options . wg21-html-filter-options))

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
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (my-wg21-html-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))

(defun my-wg21-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
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
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
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
