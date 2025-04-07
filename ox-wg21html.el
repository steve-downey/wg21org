;; ox-wg21html.el --- org exporter for WG21 papers in Latex format

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

(require 'ox-html)

(defun my-html-special-block (special-block contents info)
  "Process my special block.  SPECIAL-BLOCK CONTENTS INFO."
   (org-html-special-block special-block contents info))


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

(defun wg21-html-spec-metadata (contents info)
;;   (let ((audience (plist-get info :audience))
;;         (docnumber (plist-get info :docnumber))
;;         (email (plist-get info :email)))
;;     (concat "
;; \\docnumber{" (org-export-data docnumber info) "}
;; \\email{" (org-export-data email info) "}
;; \\audience{" (org-export-data audience info) "}\n"))
  (let* ((audience (plist-get info :audience))
         (docnumber (plist-get info :docnumber))
         (author (plist-get info :author))
         (date (plist-get info :date))
         (source_file (plist-get info :source_file))
         (source_repo (plist-get info :source_repo))
         (source_version (plist-get info :source_version))
         (git_commit (plist-get info :git_commit))
         (email (plist-get info :email))
         (link (forge-get-url :blob git_commit source_file)))
    (concat
    "
   <div data-fill-with=\"spec-metadata\">
    <dl>
     <dt>Document #: <dd> "(org-export-data docnumber info)"
     <dt>Date: <dd>" (org-export-data date info) "
     <dt>Audience: <dd>" (org-export-data audience info) "
     <dt>Reply-to: <dd><a class=\"p-name fn u-email email\" href=\"mailto:" (org-export-data email info) "\">" (org-export-data author info) " &lt;"(org-export-data email info)"&gt</a>
     <dt>Source: <dd><a href=\"" (org-export-data source_repo info) "\"/>" (org-export-data source_repo info) "</a>
                 <dd>" (org-export-data source_file info) "
                 <dd>" (org-export-data source_version info) "
    </dl>
   </div>\n"))
)

(org-export-define-derived-backend 'wg21-html 'html
  :options-alist
  '((:docnumber "DOCNUMBER" nil wg21-document-number nil)
    (:source_repo "SOURCE_REPO" nil "" nil)
    (:source_file "SOURCE_FILE" nil "" parse)
    (:source_version "SOURCE_VERSION" nil "" parse)
    (:audience "AUDIENCE" nil wg21-audience nil)
    (:toc-div-id "TOC_DIV_ID" nil wg21-toc-div-id nil))

  :translate-alist '((special-block . my-html-special-block)
                     (inner-template . my-wg21-html-inner-template)
                     (headline . my-wg21-html-headline)
                     (keyword . my-wg21-html-keyword)
                     (template . my-wg21-html-template))

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
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
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
	       (formatted-text
	        (if (plist-get info :html-self-link-headlines)
		        (format "<span class=\"content\">%s</span><a class=\"self-link\" href=\"#%s\"></a>" full-text id)
	          full-text)))
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
                  (format "\n<h%d class=\"heading\" id=\"%s\" %s>%s</h%d>\n"
                          level
                          id
			              (if (not headline-class) ""
			                (format " class=\"%s\"" headline-class))
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
