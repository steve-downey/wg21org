;;; ox-wg21reveal-test.el --- Tests for the WG21 reveal.js exporter  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.  The tests of a whole presentation need the
;; pinned reveal.js, which `make' fetches to build a talk; without it
;; they are skipped.

;;; Code:

(require 'ert)
(require 'wg21-test-support
         (expand-file-name "wg21-test-support"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'ox-wg21reveal
         (expand-file-name "../ox-wg21reveal"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

(defun ox-wg21reveal-test-export (org)
  "Export the Org text ORG with the WG21 reveal.js backend, body only."
  (let ((org-export-use-babel nil))
    (org-export-string-as org 'wg21-reveal t)))

(defun ox-wg21reveal-test-have-reveal-js ()
  "Return non-nil if the pinned reveal.js has been fetched."
  (file-readable-p (expand-file-name "dist/reveal.js" wg21-reveal-root)))

(ert-deftest ox-wg21reveal-notes-are-speaker-notes ()
  "A notes block is speaker notes, and a note block a WG21 note."
  (let ((html (ox-wg21reveal-test-export "\
* Slide
#+begin_notes
Say this.
#+end_notes
#+begin_note
Show this.
#+end_note
")))
    (should (string-match-p "<aside class=\"notes\">\\s-*<p>\\s-*Say this" html))
    (should (string-match-p "<div class=\"wg21-note\"><p>\\s-*Show this" html))))

(ert-deftest ox-wg21reveal-wording-edits ()
  "Wording on a slide marks insertions and deletions as a paper does."
  (let ((html (ox-wg21reveal-test-export "\
* Wording
#+begin_wording
#+begin_pnum
Returns [[replace:new][old]].
#+end_pnum
#+end_wording
")))
    (should (string-match-p "<del>old</del><ins>new</ins>" html))
    (should (string-match-p "class=\"pnum-number\"[^>]*data-pnum=\"1\"" html))))

(ert-deftest ox-wg21reveal-code-face-classes ()
  "Code is coloured by face classes, as in a paper, not by highlight.js."
  (let ((html (ox-wg21reveal-test-export "\
* Code
#+begin_src C++
int f();
#+end_src
")))
    (should (string-match-p "<pre class=\"src src-C\\+\\+\">" html))
    (should (string-match-p "class=\"org-type\"" html))))

(ert-deftest ox-wg21reveal-fragments ()
  "org-re-reveal's list fragments still work."
  (let ((html (ox-wg21reveal-test-export "\
* List
#+ATTR_REVEAL: :frag (appear)
- one
- two
")))
    (should (= 2 (with-temp-buffer
                   (insert html)
                   (count-matches "<li class=\"fragment appear\">"
                                  (point-min) (point-max)))))))

(ert-deftest ox-wg21reveal-title-slide ()
  "The title slide carries the paper's document block."
  (skip-unless (ox-wg21reveal-test-have-reveal-js))
  (let ((html (wg21-test-export-file 'wg21-reveal "\
#+TITLE: A Talk
#+SUBTITLE: About 50% of it
#+AUTHOR: A. Author
#+EMAIL: a@example.com
#+DATE: 2026-10-16
#+DOCNUMBER: P1234R0
#+AUDIENCE: LEWG
* One
text
")))
    (should (string-match-p "<h1 class=\"title\">A Talk</h1>" html))
    (should (string-match-p "<p class=\"subtitle\">About 50% of it</p>" html))
    (should (string-match-p "<dt>Document #:</dt><dd>P1234R0</dd>" html))
    (should (string-match-p "<dt>Audience:</dt><dd>LEWG</dd>" html))
    (should (string-match-p "mailto:a@example.com" html))))

(ert-deftest ox-wg21reveal-self-contained ()
  "A presentation embeds reveal.js and its styles; it loads nothing."
  (skip-unless (ox-wg21reveal-test-have-reveal-js))
  (let ((html (wg21-test-export-file 'wg21-reveal "\
#+TITLE: T
* One
#+begin_src C++
int f();
#+end_src
")))
    (should (string-match-p "Reveal.initialize" html))
    (should (string-match-p "/\\* wg21reveal.css \\*/" html))
    (should (string-match-p "/\\* modus-vivendi-tinted.css \\*/" html))
    (should-not (string-match-p "<script[^>]*src=" html))
    (should-not (string-match-p "<link[^>]*stylesheet" html))
    ;; The styles go in the head, after the theme.
    (should (< (string-search "wg21reveal.css" html)
               (string-search "</head>" html)))))

(provide 'ox-wg21reveal-test)
;;; ox-wg21reveal-test.el ends here
