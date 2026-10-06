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
(require 'cl-lib)
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

(defconst wg21-code-markup-commands
  '(("added" . 1) ("removed" . 1) ("replace" . 2) ("mark" . 1)
    ("emph" . 1) ("math" . 1) ("sref" . 1)
    ("exposid" . 1) ("exposidnc" . 1) ("placeholder" . 1)
    ("grammarterm" . 1) ("terminal" . 1) ("libconcept" . 1) ("tcode" . 1)
    ("seebelow" . 0) ("impdef" . 0) ("impdefnc" . 0) ("unspec" . 0))
  "Balanced escapes recognized inside raw WG21 code blocks.")

(defun wg21-code-markup--braced (text start)
  "Read one balanced braced argument in TEXT at START."
  (unless (and (< start (length text)) (= (aref text start) ?{))
    (user-error "WG21 code escape at offset %d needs a braced argument" start))
  (let ((depth 1) (position (1+ start)))
    (while (and (> depth 0) (< position (length text)))
      (pcase (aref text position)
        (?{ (setq depth (1+ depth)))
        (?} (setq depth (1- depth))))
      (setq position (1+ position)))
    (unless (= depth 0)
      (user-error "Unclosed WG21 code escape argument at offset %d" start))
    (cons (substring text (1+ start) (1- position)) position)))

(defun wg21-code-markup--arguments (text start)
  "Parse any braced arguments in TEXT from START.
Return (ARGUMENTS . END), or nil when an argument is unbalanced."
  (condition-case nil
      (let ((cursor start) arguments)
        (while (and (< cursor (length text)) (= (aref text cursor) ?{))
          (let ((argument (wg21-code-markup--braced text cursor)))
            (push (wg21-code-markup-parse (car argument)) arguments)
            (setq cursor (cdr argument))))
        (cons (nreverse arguments) cursor))
    (user-error nil)))

(defun wg21-code-markup--close (text start)
  "Return the position of the escape-closing @ in TEXT after START, or nil.
An @ inside braces belongs to a nested escape and does not close.  Once all
braces are closed, an escape cannot continue onto another code line."
  (let ((depth 0) (position start) found stopped)
    (while (and (not found) (not stopped) (< position (length text)))
      (pcase (aref text position)
        (?{ (setq depth (1+ depth)))
        (?} (setq depth (max 0 (1- depth))))
        (?\n (when (= depth 0) (setq stopped t)))
        (?@ (when (= depth 0) (setq found position))))
      (setq position (1+ position)))
    found))

(defun wg21-code-markup-parse (text)
  "Parse balanced draft escapes in raw code TEXT.
Nodes have the form (wg21-code COMMAND ARGUMENTS); ordinary text remains a
string.  Unlike the old regex substitutions, arguments may contain braces or
nested escapes.  An escape that does not take the shape \\cmd{...}...@,
such as one with an optional argument or several macros, is kept as
(wg21-code-raw LATEX NODES), where LATEX is the original text between the
@ delimiters and NODES parses any nested escapes inside it."
  (let ((position 0) nodes)
    (while (string-match "@\\\\\\([[:alpha:]]+\\)\\|\\\\ref{" text position)
      (let ((start (match-beginning 0)))
        (when (> start position)
          (push (substring text position start) nodes))
        (if (string-prefix-p "\\ref{" (match-string 0 text))
            (let* ((argument (wg21-code-markup--braced text (+ start 4))))
              (push (list 'wg21-code "ref"
                          (list (wg21-code-markup-parse (car argument)))) nodes)
              (setq position (cdr argument)))
          (let* ((command (match-string 1 text))
                 (entry (assoc command wg21-code-markup-commands))
                 (cursor (match-end 0)))
            (if entry
                (let (arguments)
                  (dotimes (_ (cdr entry))
                    (let ((argument (wg21-code-markup--braced text cursor)))
                      (push (wg21-code-markup-parse (car argument)) arguments)
                      (setq cursor (cdr argument))))
                  (if (and (< cursor (length text)) (= (aref text cursor) ?@))
                      (progn
                        (push (list 'wg21-code command (nreverse arguments))
                              nodes)
                        (setq position (1+ cursor)))
                    ;; More LaTeX follows the arguments, as in
                    ;; @\added{x}\removed{y}@: keep the escape verbatim.
                    (let ((close (wg21-code-markup--close text cursor)))
                      (unless close
                        (user-error "WG21 code escape \\%s at offset %d lacks closing @"
                                    command start))
                      (let ((latex (substring text (1+ start) close)))
                        (push (list 'wg21-code-raw latex
                                    (wg21-code-markup-parse latex))
                              nodes))
                      (setq position (1+ close)))))
              ;; Unknown commands are arbitrary LaTeX.  Keep the common
              ;; \cmd{...}...@ shape structured so nested escapes render,
              ;; and keep anything else (optional arguments, several macros)
              ;; verbatim.  A backend that cannot pass LaTeX through reports
              ;; the escape when rendering it.
              (let ((arguments (wg21-code-markup--arguments text cursor)))
                (if (and arguments
                         (< (cdr arguments) (length text))
                         (= (aref text (cdr arguments)) ?@))
                    (progn
                      (push (list 'wg21-code command (car arguments)) nodes)
                      (setq position (1+ (cdr arguments))))
                  (let ((close (wg21-code-markup--close text cursor)))
                    (if close
                        (progn
                          (let ((latex (substring text (1+ start) close)))
                            (push (list 'wg21-code-raw latex
                                        (wg21-code-markup-parse latex))
                                  nodes))
                          (setq position (1+ close)))
                      (if (car arguments)
                          (user-error "WG21 code escape \\%s at offset %d lacks closing @"
                                      command start)
                        ;; An @ followed by a C++ escape such as "mail@\\n"
                        ;; is ordinary code, not draft markup.
                        (push "@" nodes)
                        (setq position (1+ start))))))))))))
    (when (< position (length text))
      (push (substring text position) nodes))
    (nreverse nodes)))

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

(defun wg21-pnum-resolved-label (block)
  "Return BLOCK's export-time resolved paragraph label."
  (or (org-element-property :WG21_PNUM block) (wg21-pnum-label block)))

(defun wg21-pnum-anchor (block)
  "Return BLOCK's export-time paragraph anchor."
  (org-element-property :WG21_PNUM_ANCHOR block))

(defun wg21-wording-scope (element)
  "Return ELEMENT's nearest wording block or wording headline."
  (let ((ancestor (org-element-parent element)) found)
    (while (and ancestor (not found))
      (when (or (and (eq (org-element-type ancestor) 'special-block)
                     (string= (downcase (org-element-property :type ancestor))
                              "wording"))
                (and (eq (org-element-type ancestor) 'headline)
                     (org-element-property :WG21_WORDING ancestor)))
        (setq found ancestor))
      (setq ancestor (org-element-parent ancestor)))
    found))

(defun wg21-pnum--resolve (label state)
  "Resolve dotted LABEL against numbering STATE.
Return (RESOLVED . NEW-STATE)."
  (let* ((parts (split-string label "\\." nil))
         (part-count (length parts))
         (state (append (seq-take state part-count)
                        (make-list (max 0 (- part-count (length state))) '(0))))
         (index 0))
    (dolist (part parts)
      (let* ((old (or (nth index state) '(0)))
             (previous (car old))
             (previous-literal (cadr old))
             (current previous)
             literal)
        (cond
         ((string-match-p "\\`[0-9]+\\'" part)
          (setq current (string-to-number part)))
         ((string= part "#")
          (when (or (= index (1- part-count)) (= current 0) previous-literal)
            (setq current (1+ current))))
         (t (setq literal part)))
        (setf (nth index state) (list current literal))
        (unless (and (= current previous) (equal literal previous-literal))
          (let ((deeper (1+ index)))
            (while (< deeper part-count)
              (setf (nth deeper state) '(0))
              (setq deeper (1+ deeper)))))
        (setq index (1+ index))))
    (cons (mapconcat (lambda (entry)
                       (or (cadr entry) (number-to-string (car entry))))
                     state ".")
          state)))

(defun wg21-pnum-list-mode-p (scope)
  "Non-nil when SCOPE opts into paragraph-numbered Org lists."
  (and scope
       (if (eq (org-element-type scope) 'headline)
           (org-element-property :WG21_PNUM_LISTS scope)
         (string= (or (wg21-block-option scope "pnums") "") "lists"))))

(defun wg21-pnum-list-item (item)
  "Return (SCOPE PARENT-ITEM) when ITEM is a paragraph-numbered list item."
  (let ((scope (wg21-wording-scope item))
        (ancestor (org-element-parent item))
        parent-item outer-list)
    (when (wg21-pnum-list-mode-p scope)
      (while (and ancestor (not (eq ancestor scope)))
        (when (eq (org-element-type ancestor) 'item)
          (unless parent-item (setq parent-item ancestor)))
        (when (eq (org-element-type ancestor) 'plain-list)
          (setq outer-list ancestor))
        (setq ancestor (org-element-parent ancestor)))
      (when (and outer-list
                 (or (and parent-item
                          (org-element-property :WG21_PNUM parent-item))
                     (eq (org-element-property :type outer-list) 'ordered)))
        (list scope parent-item)))))

(defun wg21-resolve-paragraph-numbers (tree _backend _info)
  "Resolve automatic paragraph labels and anchors in TREE."
  (let ((states (make-hash-table :test #'eq))
        (used (make-hash-table :test #'equal))
        (serial 0))
    (cl-labels
        ((assign (element scope source-label &optional top)
           (let* ((result (wg21-pnum--resolve source-label
                                              (gethash scope states)))
                 (label (car result))
                 (stable-name (and scope
                                   (eq (org-element-type scope) 'headline)
                                   (org-element-property :CUSTOM_ID scope)))
                 (base (if stable-name
                           (format "%s-%s" stable-name label)
                         (format "pnum-%d" (setq serial (1+ serial)))))
                 (count (1+ (gethash base used 0)))
                 (anchor (if (= count 1) base (format "%s-%d" base count))))
            (puthash scope (cdr result) states)
            (puthash base count used)
            (org-element-put-property element :WG21_PNUM label)
            (org-element-put-property element :WG21_PNUM_ANCHOR anchor)
            (when top (org-element-put-property element :WG21_PNUM_TOP t)))))
      (org-element-map tree '(special-block item)
        (lambda (element)
          (pcase (org-element-type element)
            ('special-block
             (when (string= (downcase (org-element-property :type element)) "pnum")
               (assign element (wg21-wording-scope element)
                       (or (wg21-pnum-label element) "#"))))
            ('item
             (when-let* ((context (wg21-pnum-list-item element)))
               (let* ((scope (car context))
                      (parent (cadr context))
                      (component (if-let* ((counter (org-element-property
                                                      :counter element)))
                                     (number-to-string counter)
                                   "#"))
                      (label (if parent
                                 (concat (org-element-property :WG21_PNUM parent)
                                         "." component)
                               component)))
                 (assign element scope label (null parent)))))))))
    tree))

(defun wg21-local-stable-name-p (stable-name info)
  "Non-nil when INFO's document defines STABLE-NAME as a CUSTOM_ID."
  (org-element-map
      (plist-get info :parse-tree) 'headline
    (lambda (headline)
      (equal stable-name (org-element-property :CUSTOM_ID headline)))
    info t))

(defun wg21-stable-name-href (stable-name info)
  "Return the HTML target for STABLE-NAME in export context INFO.
Use a local CUSTOM_ID when this paper defines the stable name, and the
current working draft otherwise."
  (if (wg21-local-stable-name-p stable-name info)
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
