;; ox-wg21reveal.el --- org exporter for WG21 talks as reveal.js slides  -*- lexical-binding: t; -*-

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

;; A talk about a paper is written in the same Org as the paper, and
;; exported by org-re-reveal as a reveal.js presentation.  This backend
;; derives from org-re-reveal, which makes the slides, and borrows the
;; WG21 HTML exporter's transcoders for what goes on them: wording,
;; notes and examples, comparison tables, grammar, code, citations, and
;; math.
;;
;; Like a paper, a presentation is one self-contained file: reveal.js,
;; its theme, the plugins, the stylesheets, and the images are all
;; embedded, so it can be presented from a laptop with no network.
;; reveal.js itself is not bundled; the Makefile fetches a pinned
;; release into `wg21-reveal-root'.

;;; Code:

(require 'cl-lib)
(require 'org-re-reveal)
(require 'ox-wg21html
         (expand-file-name "ox-wg21html"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

(defgroup wg21-reveal nil
  "Options for exporting WG21 talks as reveal.js presentations."
  :group 'my-export-wg21)

(defcustom wg21-reveal-root
  (expand-file-name ".deps/reveal.js" wg21-html-directory)
  "The reveal.js release the presentations embed, set per talk by #+REVEAL_ROOT.
`make' fetches the release the Makefile pins to here."
  :group 'wg21-reveal
  :type 'directory)

(defcustom wg21-reveal-theme "white"
  "The reveal.js theme, set per talk by #+REVEAL_THEME.
Only the white, black, and contrast themes embed their fonts; the
others import them from Google, and `make check' reports them."
  :group 'wg21-reveal
  :type 'string)

(defcustom wg21-reveal-plugins '(notes search zoom)
  "The reveal.js plugins embedded in a talk, set by #+REVEAL_PLUGINS.
Code is coloured by Emacs as it is in a paper, so the highlight
plugin is not needed, and math is MathML, so the math plugin is not
needed either."
  :group 'wg21-reveal
  :type '(set (const notes) (const search) (const zoom)
              (const markdown) (const highlight) (const math)))

(defcustom wg21-reveal-style '("wg21reveal.css")
  "Stylesheets embedded after the theme, set per talk by #+WG21_REVEAL_STYLE.
A relative name is looked up next to the talk, then next to this
exporter."
  :group 'wg21-reveal
  :type '(repeat string))

;;; Transcoders

(defun wg21-reveal-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK: WG21 blocks as in a paper, others as slides.
So #+begin_notes is speaker notes, and #+begin_note a WG21 note.
CONTENTS and INFO have their usual meanings."
  (my-html-special-block special-block contents info
                         #'org-re-reveal-special-block))

(defun wg21-reveal-item (item contents info)
  "Transcode ITEM, numbered as wording or as a slide's list item.
CONTENTS and INFO have their usual meanings."
  (wg21-html-item item contents info #'org-re-reveal-item))

(defun wg21-reveal-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST, numbered as wording or with slide fragments.
CONTENTS and INFO have their usual meanings."
  (wg21-html-plain-list plain-list contents info #'org-re-reveal-plain-list))

(defun wg21-reveal-src-block (src-block contents info)
  "Transcode SRC-BLOCK as in a paper, set as a slide's code.
CONTENTS and INFO have their usual meanings."
  (wg21-html-src-block src-block contents info #'org-re-reveal-src-block))

;;; Title slide

(defun wg21-reveal--format-escape (html)
  "Return HTML with each % doubled, for `format-spec'."
  (replace-regexp-in-string "%" "%%" html t t))

(defun wg21-reveal--title-slide (info)
  "Return the title slide, as a `format-spec' string, for INFO.
The title and subtitle, then the paper's document block: its number,
the date, the audience, and who to reply to."
  (let* ((data (lambda (key) (org-trim (org-export-data (plist-get info key) info))))
         (row (lambda (term value)
                (unless (string-empty-p value)
                  (format "<dt>%s</dt><dd>%s</dd>\n" term value))))
         (docnumber (funcall data :docnumber))
         (subtitle (funcall data :subtitle))
         (author (funcall data :author))
         (email (funcall data :email)))
    (concat
     "<h1 class=\"title\">%t</h1>\n"
     (unless (string-empty-p subtitle)
       "<p class=\"subtitle\">%s</p>\n")
     "<dl class=\"wg21-slide-meta\">\n"
     (wg21-reveal--format-escape
      (concat
       (unless (string= docnumber wg21-document-number)
         (funcall row "Document #:" docnumber))
       (and (plist-get info :with-date)
            (funcall row "Date:" (funcall data :date)))
       (funcall row "Audience:" (funcall data :audience))
       (and (plist-get info :with-author)
            (funcall row "Reply-to:"
                     (if (string-empty-p email)
                         author
                       (format "%s &lt;<a href=\"mailto:%s\">%s</a>&gt;"
                               author email email))))))
     "</dl>\n")))

;;; Template

(defun wg21-reveal--styles (contents info)
  "Return the <style> elements a talk embeds after its reveal.js theme.
The talk's stylesheets, then the face stylesheet for its code, with
rules for faces CONTENTS does not use left out.  Slides have no Theme
switch, so only the first of #+WG21_CODE_STYLE applies.  INFO is a
plist holding export options."
  (let ((classes (wg21-html--used-classes contents)))
    (mapconcat
     (lambda (name)
       (let ((file (wg21-html--find-css name info)))
         (unless file (user-error "Stylesheet %s not found" name))
         (wg21-html--style-element file classes)))
     (append (plist-get info :wg21-reveal-style)
             (seq-take (plist-get info :wg21-code-style) 1))
     "")))

(defun wg21-reveal-template (contents info)
  "Return the complete presentation for CONTENTS.
That is org-re-reveal's, with a WG21 title slide unless the talk
gives its own, the WG21 styles, and images and citations treated as
in a paper.  INFO is a plist holding export options."
  (when (eq (plist-get info :reveal-title-slide) 'auto)
    (plist-put info :reveal-title-slide (wg21-reveal--title-slide info)))
  (let* ((styles (wg21-reveal--styles contents info))
         (html (org-re-reveal-template
                (wg21-html--link-citations
                 (wg21-html--embed-images contents info))
                info))
         ;; The first, before the embedded scripts.
         (head-end (string-search "</head>" html)))
    (concat (substring html 0 head-end) styles (substring html head-end))))

(org-export-define-derived-backend 'wg21-reveal 're-reveal
  :options-alist
  '((:docnumber "DOCNUMBER" nil wg21-document-number nil)
    (:source_repo "SOURCE_REPO" nil "" nil)
    (:source_file "SOURCE_FILE" nil "" parse)
    (:source_version "SOURCE_VERSION" nil "" parse)
    (:git_commit "GIT_COMMIT" nil "" parse)
    (:audience "AUDIENCE" nil wg21-audience nil)
    (:project "PROJECT" nil wg21-project nil)
    (:wg21-code-style "WG21_CODE_STYLE" nil wg21-html-code-style split)
    (:wg21-code-language "WG21_CODE_LANGUAGE" nil wg21-code-language nil)
    (:wg21-cpp-keywords "WG21_CPP_KEYWORDS" nil nil split)
    (:wg21-reveal-style "WG21_REVEAL_STYLE" nil wg21-reveal-style split)
    ;; Only an address the talk gives, not the exporting user's.
    (:email "EMAIL" nil "" t)
    (:html-wrap-src-lines nil nil org-html-wrap-src-lines)
    ;; A talk is one file presented offline, from a pinned reveal.js.
    (:reveal-single-file nil "reveal_single_file" t t)
    (:reveal-root "REVEAL_ROOT" nil wg21-reveal-root t)
    (:reveal-version "REVEAL_VERSION" nil "6" t)
    (:reveal-theme "REVEAL_THEME" nil wg21-reveal-theme t)
    (:reveal-plugins "REVEAL_PLUGINS" nil wg21-reveal-plugins t)
    ;; Slides are found by position, not by number or a contents slide.
    (:with-toc nil "toc" nil)
    (:section-numbers nil "num" nil))

  :translate-alist '((special-block . wg21-reveal-special-block)
                     (code . wg21-html-code)
                     (verbatim . wg21-html-verbatim)
                     (item . wg21-reveal-item)
                     (plain-list . wg21-reveal-plain-list)
                     (table . wg21-html-table)
                     (src-block . wg21-reveal-src-block)
                     (inline-src-block . wg21-html-inline-src-block)
                     (latex-fragment . wg21-html-latex-fragment)
                     (latex-environment . wg21-html-latex-environment)
                     (template . wg21-reveal-template))

  :filters-alist '((:filter-options . wg21-html-filter-options)
                   (:filter-parse-tree . (wg21-code-apply-default-language
                                          wg21-cite-add-bibliography
                                          wg21-resolve-paragraph-numbers
                                          wg21-seed-headline-references
                                          wg21-cite-diagnose-paper-revisions
                                          wg21-cite-drop-empty-bibliography)))

  :menu-entry '(?w "Export WG21 Paper"
                   ((?S "As reveal.js slides" wg21-reveal-export-to-html)
                    (?s "As reveal.js slides and open"
                        (lambda (a s v b)
                          (if a (wg21-reveal-export-to-html t s v b)
                            (org-open-file (wg21-reveal-export-to-html nil s v b))))))))

;;; End-user functions

;;;###autoload
(defun wg21-reveal-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current buffer as a reveal.js presentation in a file.
A talk in talk.slides.org becomes talk.slides.html.  ASYNC,
SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST are as for
`my-wg21-export-to-html'.  Return the output file's name."
  (interactive)
  (when (fboundp 'wg21org-enable-transclusion)
    (wg21org-enable-transclusion))
  (let ((file (org-export-output-file-name ".html" subtreep))
        (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'wg21-reveal file
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-wg21reveal)
;;; ox-wg21reveal.el ends here
