;; wg21-cite.el --- citations and bibliography for WG21 papers  -*- lexical-binding: t; -*-

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

;; Shared by the HTML and LaTeX exporters:
;;
;; - A paper that cites nothing loses its References heading, rather
;;   than exporting it empty, when all the heading holds is the
;;   #+PRINT_BIBLIOGRAPHY: that would have filled it.
;;
;; - A citation of a reference with exactly one URL links to that URL,
;;   rather than to the reference's entry at the end of the paper.
;;   Each exporter finds the entries and citations in its own output
;;   and uses `wg21-cite-single-urls' to decide.
;;
;; - The bundled wg21.bib is the default bibliography.  When a paper has
;;   citations but no #+PRINT_BIBLIOGRAPHY, a References section is added.
;;
;; - [[cite-title:P2996R8]] cites a paper with its title.  This is an Org
;;   link because Org citation styles deliberately do not include a
;;   Markdown-like class mechanism.

;;; Code:

(require 'org-element)
(require 'ox)
(require 'ol)
(require 'bibtex)

(defconst wg21-cite-default-bibliography
  (expand-file-name "wg21.bib"
                    (file-name-directory (or (macroexp-file-name) buffer-file-name)))
  "WG21's downloaded paper index, used when a paper names no bibliography.")

(defun wg21-cite-default-options (info)
  "Give export INFO the bundled WG21 bibliography when it has none."
  (unless (plist-get info :bibliography)
    (when (file-readable-p wg21-cite-default-bibliography)
      (plist-put info :bibliography (list wg21-cite-default-bibliography))
      ;; Some citation processors ask Org for the buffer bibliography again
      ;; after the export options have been collected.
      (setq-local org-cite-global-bibliography
                  (cons wg21-cite-default-bibliography
                        org-cite-global-bibliography))))
  info)

(defun wg21-cite-add-bibliography (tree _backend info)
  "Add title citations and a default References section to TREE.
A cite-title link gets a no-output Org citation so its key participates in
the bibliography.  Add the section itself only when the paper has no
#+PRINT_BIBLIOGRAPHY keyword."
  (let ((has-citations (org-element-map tree 'citation #'identity info t))
        (has-print (org-element-map
                       tree 'keyword
                     (lambda (keyword)
                       (string= (org-element-property :key keyword)
                                "PRINT_BIBLIOGRAPHY"))
                     info t))
        title-keys)
    (org-element-map tree 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "cite-title")
          (let ((key (org-element-property :path link)))
            (unless (string-match-p "\\`[[:alnum:]-]+\\'" key)
              (user-error "Invalid WG21 citation key: %s" key))
            (push key title-keys)))) info)
    (when title-keys
      (let ((fragment
             (with-temp-buffer
               (org-mode)
               (insert (format "[cite/nocite:%s]\n"
                               (mapconcat (lambda (key) (concat "@" key))
                                          (delete-dups title-keys) ";")))
               (org-element-parse-buffer))))
        (apply #'org-element-adopt-elements tree (org-element-contents fragment))))
    (when (and (or has-citations title-keys) (not has-print))
      (let ((fragment
             (with-temp-buffer
               (org-mode)
               (insert "* References\n#+PRINT_BIBLIOGRAPHY:\n")
               (org-element-parse-buffer))))
        (apply #'org-element-adopt-elements tree (org-element-contents fragment)))))
  tree)

(defun wg21-cite--entry (key bibliography)
  "Return parsed BibTeX entry KEY from BIBLIOGRAPHY, or nil."
  (when (file-readable-p bibliography)
    (with-temp-buffer
      (insert-file-contents bibliography)
      (bibtex-mode)
      (when (bibtex-search-entry key)
        (bibtex-parse-entry t)))))

(defun wg21-cite--plain-bibtex (text)
  "Turn the small amount of BibTeX markup in TEXT into readable prose."
  (when text
    (setq text (replace-regexp-in-string "[{}]" "" text))
    (setq text (replace-regexp-in-string "\\\\&" "&" text t t))
    text))

(defun wg21-cite-title (key info)
  "Return the title of bibliography entry KEY in INFO, or nil."
  (seq-some
   (lambda (bibliography)
     (when-let* ((entry (wg21-cite--entry key bibliography)))
       (wg21-cite--plain-bibtex (cdr (assoc-string "title" entry t)))))
   (or (plist-get info :bibliography)
       (list wg21-cite-default-bibliography))))

(defun wg21-cite--title-export (key description backend info)
  "Export a title citation for KEY with optional DESCRIPTION."
  (unless (string-match-p "\\`[[:alnum:]-]+\\'" key)
    (user-error "Invalid WG21 citation key: %s" key))
  (let* ((bib-title (and (not description) (wg21-cite-title key info)))
         (_ (unless (or description bib-title)
              (user-error "Citation %s is absent from the bibliography index" key)))
         (url (format "https://wg21.link/%s" (downcase key))))
    (cond
     ((org-export-derived-backend-p backend 'html)
      (format "<a href=\"%s\">[%s] (%s)</a>" url
              (org-html-encode-plain-text key)
              (or description (org-html-encode-plain-text bib-title))))
     ((org-export-derived-backend-p backend 'latex)
      (format "\\href{%s}{[%s] (%s)}" url
              (org-latex-plain-text key info)
              (or description (org-latex-plain-text bib-title info))))
     (t (format "[%s] (%s)" key (or description bib-title))))))

(org-link-set-parameters "cite-title" :export #'wg21-cite--title-export)

(defun wg21-cite-diagnose-paper-revisions (tree _backend info)
  "Warn about missing or superseded WG21 P-paper keys cited in TREE."
  (let ((latest (make-hash-table :test #'equal))
        keys)
    (org-element-map tree 'citation
      (lambda (citation)
        (setq keys (append (org-cite-get-references citation t) keys))) info)
    (org-element-map tree 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "cite-title")
          (push (org-element-property :path link) keys))) info)
    (when keys
      (when (file-readable-p wg21-cite-default-bibliography)
        (with-temp-buffer
          (insert-file-contents wg21-cite-default-bibliography)
          (goto-char (point-min))
          (let ((case-fold-search t))
            (while (re-search-forward
                    "^@[^{]+{\\(P\\([0-9]+\\)R\\([0-9]+\\)\\)," nil t)
              (let* ((key (upcase (match-string 1)))
                     (series (match-string 2))
                     (revision (string-to-number (match-string 3)))
                     (old (gethash series latest)))
                (when (or (not old) (> revision (cdr old)))
                  (puthash series (cons key revision) latest)))))))
      (dolist (key (delete-dups keys))
        (when (string-match "\\`[Pp]\\([0-9]+\\)\\(?:[Rr]\\([0-9]+\\)\\)?\\'" key)
          (let* ((series (match-string 1 key))
                 (revision (and (match-string 2 key)
                                (string-to-number (match-string 2 key))))
                 (newest (gethash series latest)))
            (cond
             ((not newest)
              (message "WG21 citation %s is absent from wg21.bib; refresh the index" key))
             ((not revision)
              (message "WG21 citation %s has no revision; latest indexed revision is %s"
                       key (car newest)))
             ((< revision (cdr newest))
              (message "WG21 citation %s is older than indexed %s"
                       key (car newest)))))))))
  tree)

(defconst wg21-cite--exported-keywords '("HTML" "LATEX" "TOC")
  "Keywords that put something in the output.  Others only configure it.")

(defun wg21-cite--empty-section-p (section)
  "Non-nil if SECTION holds nothing but keywords that export nothing."
  (seq-every-p (lambda (element)
                 (and (eq (org-element-type element) 'keyword)
                      (not (member (org-element-property :key element)
                                   wg21-cite--exported-keywords))))
               (org-element-contents section)))

(defun wg21-cite-drop-empty-bibliography (tree _backend info)
  "Drop the bibliography from TREE when the paper cites nothing.
Each #+PRINT_BIBLIOGRAPHY: is removed, and so is a headline left with
nothing to export, such as a References heading holding only the
bibliography and its #+CITE_EXPORT.  A parse tree filter; INFO is the
export plist.  Return TREE."
  (unless (org-element-map tree 'citation #'identity info t)
    (org-element-map tree 'keyword
      (lambda (keyword)
        (when (string= (org-element-property :key keyword) "PRINT_BIBLIOGRAPHY")
          (let* ((section (org-element-parent keyword))
                 (headline (and (eq (org-element-type section) 'section)
                                (org-element-parent section))))
            (org-element-extract keyword)
            (when (and headline
                       (eq (org-element-type headline) 'headline)
                       (wg21-cite--empty-section-p section)
                       (equal (org-element-contents headline) (list section)))
              (org-element-extract headline)))))
      info))
  tree)

(defun wg21-cite-single-urls (entries)
  "Return a hash table from reference to URL, for references with one URL.
ENTRIES is a list of (REFERENCE . URLS).  A reference with no URL, or
with more than one, is left out: its citation keeps linking to the
bibliography."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((urls (delete-dups (copy-sequence (cdr entry)))))
        (when (= 1 (length urls))
          (puthash (car entry) (car urls) table))))
    table))

(defun wg21-cite-urls (entry)
  "Return the http(s) URLs in the bibliography ENTRY, in order.
CSL styles print a reference's URL as a link or as plain text, WG21
papers usually the latter, so both count.  Punctuation that ends the
sentence is not part of the URL."
  (mapcar (lambda (url) (replace-regexp-in-string "[.,;:)]+\\'" "" url))
          (wg21-cite-matches "https?://[^][[:space:]<>\"{}\\\\]+" entry)))

(defun wg21-cite-matches (regexp string &optional group)
  "Return the matches of REGEXP in STRING, or of its GROUP."
  (let ((start 0) matches)
    (while (string-match regexp string start)
      (push (match-string (or group 0) string) matches)
      (setq start (match-end 0)))
    (nreverse matches)))

(provide 'wg21-cite)
;;; wg21-cite.el ends here
