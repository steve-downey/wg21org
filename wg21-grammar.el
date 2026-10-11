;; wg21-grammar.el --- grammar productions in WG21 papers  -*- lexical-binding: t; -*-

;;; Commentary:

;; A grammar block holds productions written the way the working draft
;; prints them:
;;
;;   #+begin_grammar
;;   escaped-identifier:
;;       ` identifier `
;;
;;   sign: one of
;;       + -
;;   #+end_grammar
;;
;; A line at the left margin ending in a colon defines a nonterminal; an
;; indented line is one alternative; a blank line starts the next
;; production.  Words are nonterminals and runs of punctuation are
;; terminals.  A keyword is a terminal that looks like a word, so it is
;; written as Org verbatim, =typename=.  Every token of a "one of"
;; production is a terminal.  WORD_{opt} is an optional element.  A line
;; that is only "..." elides alternatives that are not shown.
;;
;; Org's insert, delete, replace and mark links work inside a line, as do
;; the draft escapes of the other raw wording blocks.

;;; Code:

(require 'subr-x)
(require 'url-util)
(require 'wg21-wording
         (expand-file-name "wg21-wording"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

(defconst wg21-grammar--identifier
  "[[:alpha:]][[:alnum:]]*\\(?:[-_][[:alnum:]]+\\)*"
  "A nonterminal name: words joined by hyphens.")

(defconst wg21-grammar--verbatim
  "\\([=~]\\)\\([^ \t=~]\\|[^ \t=~][^\n]*?[^ \t]\\)\\1"
  "An Org verbatim or code span, which marks a terminal.")

(defun wg21-grammar--links-to-escapes (line)
  "Rewrite Org editing links in LINE as the equivalent draft escapes."
  (replace-regexp-in-string
   "\\[\\[\\(insert\\|delete\\|mark\\|replace\\):\\([^]]*\\)\\]\\[\\([^]]*\\)\\]\\]"
   (lambda (link)
     (let ((kind (match-string 1 link))
           (target (url-unhex-string (match-string 2 link)))
           (text (match-string 3 link)))
       (pcase kind
         ("insert" (format "@\\added{%s}@" text))
         ("delete" (format "@\\removed{%s}@" text))
         ("mark" (format "@\\mark{%s}@" text))
         ("replace" (format "@\\replace{%s}{%s}@" text target)))))
   line t t))

(defun wg21-grammar--boundary-p (text position)
  "Non-nil if POSITION in TEXT starts or ends a token."
  (or (<= position 0) (>= position (length text))
      (memq (aref text position) '(?\s ?\t))
      (memq (aref text (1- position)) '(?\s ?\t))))

(defun wg21-grammar--tokens (text all-terminals)
  "Classify the plain TEXT of a grammar line into grammar nodes.
Nodes are (wg21-grammar-terminal STRING), (wg21-grammar-nonterminal
STRING), (wg21-grammar-opt NODE), and (wg21-grammar-space STRING FIXED).
ALL-TERMINALS makes words terminals too, as in a \"one of\" production,
and FIXED says the space keeps its width so that rows line up."
  (let ((position 0) nodes)
    (while (< position (length text))
      (let (node end)
        (cond
         ((eq (string-match "[ \t]+" text position) position)
          (setq node (list 'wg21-grammar-space (match-string 0 text) all-terminals)
                end (match-end 0)))
         ((and (wg21-grammar--boundary-p text position)
               (eq (string-match wg21-grammar--verbatim text position) position)
               (let ((after (match-end 0)))
                 (or (wg21-grammar--boundary-p text after)
                     (string-prefix-p "_{opt}" (substring text after)))))
          (setq node (list 'wg21-grammar-terminal (match-string 2 text))
                end (match-end 0)))
         ((eq (string-match wg21-grammar--identifier text position) position)
          (setq node (list (if all-terminals 'wg21-grammar-terminal
                             'wg21-grammar-nonterminal)
                           (match-string 0 text))
                end (match-end 0)))
         ((eq (string-match "[^[:space:][:alnum:]]+" text position) position)
          (setq node (list 'wg21-grammar-terminal (match-string 0 text))
                end (match-end 0)))
         (t
          (setq node (list 'wg21-grammar-terminal (substring text position (1+ position)))
                end (1+ position))))
        (when (string-prefix-p "_{opt}" (substring text end))
          (setq node (list 'wg21-grammar-opt node)
                end (+ end 6)))
        (push node nodes)
        (setq position end)))
    (nreverse nodes)))

(defun wg21-grammar--classify (nodes all-terminals)
  "Classify the plain text in parsed escape NODES as grammar.
Text inside editing escapes is grammar too; the arguments of any other
escape are left as written."
  (mapcan
   (lambda (node)
     (cond
      ((stringp node) (wg21-grammar--tokens node all-terminals))
      ((and (eq (car node) 'wg21-code)
            (member (nth 1 node) '("added" "removed" "replace" "mark")))
       (list (list 'wg21-code (nth 1 node)
                   (mapcar (lambda (argument)
                             (wg21-grammar--classify argument all-terminals))
                           (nth 2 node)))))
      (t (list node))))
   nodes))

(defun wg21-grammar--line-nodes (text all-terminals)
  "Return grammar nodes for the line TEXT."
  (wg21-grammar--classify
   (wg21-code-markup-parse (wg21-grammar--links-to-escapes text))
   all-terminals))

(defun wg21-grammar-parse (text)
  "Parse the grammar block TEXT into a list of productions.
Each production is a list of lines:
  (definition NODES ONE-OF)  a nonterminal being defined
  (alternative NODES)        one right-hand side
  (elision)                  alternatives left out
A line before any definition is an alternative of an unnamed production."
  (let (productions lines one-of)
    (dolist (line (split-string (string-trim-right text) "\n"))
      (cond
       ((string-blank-p line)
        (when lines (push (nreverse lines) productions))
        (setq lines nil one-of nil))
       ((string-match-p "\\`[ \t]*\\(\\.\\.\\.\\|…\\)[ \t]*\\'" line)
        (push (list 'elision) lines))
       ((string-match "\\`\\([^ \t].*?\\)[ \t]*:\\([ \t]+one of\\)?[ \t]*\\'" line)
        (when lines (push (nreverse lines) productions))
        (setq one-of (and (match-beginning 2) t)
              lines (list (list 'definition
                                (wg21-grammar--line-nodes (match-string 1 line) nil)
                                one-of))))
       (t
        (push (list 'alternative
                    (wg21-grammar--line-nodes (string-trim line) one-of))
              lines))))
    (when lines (push (nreverse lines) productions))
    (nreverse productions)))

(provide 'wg21-grammar)
;;; wg21-grammar.el ends here
