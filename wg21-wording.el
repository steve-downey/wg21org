;; wg21-wording.el --- proposed wording in WG21 papers  -*- lexical-binding: t; -*-

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

;; Proposed wording is a #+begin_wording block, the wording environment
;; of stdtex/paper_macros.tex, which numbers its sections the way the
;; working draft does.  In wording, colour means an edit: green inserted,
;; red removed.  So code there is not syntax highlighted, which would
;; put the same colours on text that is not an edit; each exporter sets
;; it the way the working draft sets code.

;;; Code:

(require 'org-element)
(require 'seq)
(defun wg21--headline-slug (headline info)
  "Return a readable anchor derived from HEADLINE's title.
INFO is the current export state.  The spelling follows Pandoc's
auto-identifier convention closely enough to preserve the anchors of
papers converted from Markdown: markup and punctuation disappear,
whitespace becomes hyphens, and underscores remain significant."
  (ignore info)
  (let* ((title (substring-no-properties
                 (org-element-interpret-data
                  (org-element-property :title headline))))
         (slug (downcase title)))
    (setq slug (replace-regexp-in-string "[[:space:]]+" "-" slug))
    (setq slug (replace-regexp-in-string "[^[:alnum:]_-]" "" slug))
    (setq slug (replace-regexp-in-string "\\`[-_]+\\|[-_]+\\'" "" slug))
    (if (string-empty-p slug) "section" slug)))

(defun wg21-seed-headline-references (tree _backend info)
  "Give ordinary headlines in TREE stable, readable export references.
INFO is the current export state.  An explicit CUSTOM_ID is reserved and
left untouched; this is how generated wording retains standard stable
names.  Repeated derived names receive -2, -3, and so on."
  (let ((used (make-hash-table :test #'equal))
        (references (plist-get info :internal-references)))
    (org-element-map tree 'headline
      (lambda (headline)
        (when-let* ((custom-id (org-element-property :CUSTOM_ID headline)))
          (puthash custom-id 1 used))))
    (org-element-map tree 'headline
      (lambda (headline)
        (unless (org-element-property :CUSTOM_ID headline)
          (let* ((base (wg21--headline-slug headline info))
                 (count (1+ (gethash base used 0)))
                 (reference (if (= count 1) base
                              (format "%s-%d" base count))))
            (while (gethash reference used)
              (setq count (1+ count)
                    reference (format "%s-%d" base count)))
            (puthash base count used)
            (puthash reference 1 used)
            (push (cons reference headline) references)))))
    (plist-put info :internal-references references)
    tree))

(defun wg21-wording-p (element)
  "Non-nil if ELEMENT is inside a #+begin_wording block."
  (let ((ancestor (org-element-parent element)) found)
    (while (and ancestor (not found))
      (setq found (or (and (eq (org-element-type ancestor) 'special-block)
                           (string= (downcase (org-element-property :type ancestor))
                                    "wording"))
                      (and (eq (org-element-type ancestor) 'headline)
                           (org-element-property :WG21_WORDING ancestor)))
            ancestor (org-element-parent ancestor)))
    found))

(defun wg21-raw-code-block-p (element)
  "Non-nil if ELEMENT is inside a raw WG21 code or grammar block."
  (let ((ancestor (org-element-parent element)) found)
    (while (and ancestor (not found))
      (setq found (and (eq (org-element-type ancestor) 'special-block)
                       (member (downcase (org-element-property :type ancestor))
                               '("codeblock" "itemdecl" "grammar")))
            ancestor (org-element-parent ancestor)))
    found))

(defun wg21-special-block-raw-contents (block)
  "Return BLOCK's contents exactly as written, without Org interpretation."
  (let ((begin (org-element-property :contents-begin block))
        (end (org-element-property :contents-end block)))
    (if (and begin end)
        (buffer-substring-no-properties begin end)
      "")))

(defun wg21-block-arguments (block)
  "Return BLOCK's whitespace-separated parameters.
Quoting follows ordinary Emacs command-line quoting, which is enough for
values such as an editorial note's audience."
  (split-string-and-unquote (or (org-element-property :parameters block) "")))

(defun wg21-block-option (block name)
  "Return option NAME from BLOCK's parameters, or nil.
Both `:name value' and `name=value' are accepted."
  (let ((args (wg21-block-arguments block)) value)
    (while args
      (let ((arg (pop args)))
        (cond
         ((string= arg (concat ":" name))
          (setq value (pop args) args nil))
         ((string-prefix-p (concat name "=") arg)
          (setq value (substring arg (1+ (length name))) args nil)))))
    value))

(defun wg21-block-flag-p (block name)
  "Non-nil when BLOCK has the flag NAME."
  (member name (wg21-block-arguments block)))

(defun wg21-pnum-label (block)
  "Return BLOCK's explicit paragraph label, or nil for automatic numbering.
The compact `#+begin_pnum x+1' form and `:number x+1' are equivalent."
  (or (wg21-block-option block "number")
      (seq-find (lambda (arg) (not (string-prefix-p ":" arg)))
                (wg21-block-arguments block))))

(defun wg21-stable-name-href (stable-name info)
  "Return the HTML target for STABLE-NAME in export context INFO.
Use a local CUSTOM_ID when this paper defines the stable name, and the
current working draft otherwise."
  (if (org-element-map
          (plist-get info :parse-tree) 'headline
        (lambda (headline)
          (equal stable-name (org-element-property :CUSTOM_ID headline)))
        info t)
      (concat "#" stable-name)
    (concat "https://eel.is/c++draft/" stable-name)))

(defun wg21-table-columns (table)
  "Return TABLE's target-neutral WG21 column widths, or nil.
The widths are positive integer percentages from an ATTR_WG21
`:columns' attribute.  Signal an error for malformed metadata so a
generated table cannot silently acquire a different layout."
  (when-let* ((columns (org-export-read-attribute :attr_wg21 table :columns))
              (widths (split-string columns "[ 	]+" t)))
    (unless (seq-every-p (lambda (width)
                           (string-match-p "\\`[1-9][0-9]*\\'" width))
                         widths)
      (user-error "Invalid ATTR_WG21 column widths: %s" columns))
    widths))

(provide 'wg21-wording)
;;; wg21-wording.el ends here
