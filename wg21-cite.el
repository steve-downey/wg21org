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

;;; Code:

(require 'org-element)
(require 'ox)

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
