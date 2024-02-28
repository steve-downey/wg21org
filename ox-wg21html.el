(require 'ox-html)

;;; Code:

(defun my-html-special-block (special-block contents info)
  "Process my special block.  SPECIAL-BLOCK CONTENTS INFO."
   (org-html-special-block special-block contents info))


(defun my-wg21-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer."
  (interactive)
  (let ((file (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'wg21-html file
      async subtreep visible-only body-only ext-plist)))


(defcustom wg21-document-number "Dnnnn"
  "doc string"
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-audience "WG21"
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
  (let ((audience (plist-get info :audience))
        (docnumber (plist-get info :docnumber))
        (author (plist-get info :author))
        (date (plist-get info :date))
        (source_file (plist-get info :source_file))
        (source_repo (plist-get info :source_repo))
        (source_version (plist-get info :source_version))
        (email (plist-get info :email)))
    (concat
    "
   <div data-fill-with=\"spec-metadata\">
    <dl>
     <dt>Document #: <dd> "(org-export-data docnumber info)"
     <dt>Date: <dd>" (org-export-data date info) "
     <dt>Audience: <dd>" (org-export-data audience info) "
     <dt>Reply-to: <dd><a class=\"p-name fn u-email email\" href=\"mailto:" (org-export-data email info) "\">" (org-export-data author info) "</a>
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
    (:audience "AUDIENCE" nil wg21-audience nil))

  :translate-alist '((special-block . my-html-special-block)
                     (template . my-wg21-html-template))

  :menu-entry '(?w "WG21 Papers" ((?h "wg21 html" my-wg21-export-to-html))))


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




(provide 'ox-wg21html)
;;; ox-wg21html.el ends here
