;;; ox-wg21html-test.el --- Tests for the WG21 HTML exporter  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'wg21-test-support
         (expand-file-name "wg21-test-support"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'ox-wg21html
         (expand-file-name "../ox-wg21html"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

(defun ox-wg21html-test-export (org)
  "Export the Org text ORG with the WG21 HTML backend, body only."
  (let ((org-export-use-babel nil))
    (org-export-string-as org 'wg21-html t)))

(defun ox-wg21html-test-count (regexp string)
  "Count the matches of REGEXP in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defconst ox-wg21html-test-cmptbl "\
#+begin_cmptbl
#+begin_cmptblcell before
*Before*
#+end_cmptblcell
#+begin_cmptblcell after
*After*
#+end_cmptblcell
#+begin_cmptblcell before
#+begin_src C++
int a = 1;
int b = 2;
#+end_src
#+end_cmptblcell
#+begin_cmptblcell after
#+begin_src C++
auto [a, b] = pair(1, 2);
#+end_src
#+end_cmptblcell
#+begin_cmptblcell before
#+begin_src C++
x();
#+end_src
#+end_cmptblcell
#+begin_cmptblcell after
#+begin_src C++
y();
#+end_src
#+end_cmptblcell
#+end_cmptbl
"
  "A comparison table with a header row and two rows of code.")

(ert-deftest cmptbl-is-a-table ()
  (let ((html (ox-wg21html-test-export ox-wg21html-test-cmptbl)))
    (should (string-match-p "<table class=\"cmptbl\">" html))
    (should-not (string-match-p "class=\"cmptblcell\"" html))
    (should (= 3 (ox-wg21html-test-count "<tr>" html)))
    (should (= 2 (ox-wg21html-test-count "<td class=\"cmptbl-before\">" html)))
    (should (= 2 (ox-wg21html-test-count "<td class=\"cmptbl-after\">" html)))))

(ert-deftest cmptbl-header-row ()
  (let ((html (ox-wg21html-test-export ox-wg21html-test-cmptbl)))
    (should (string-match-p
             "<thead>\n<tr><th class=\"cmptbl-before\">.*Before.*</th><th class=\"cmptbl-after\">.*After.*</th></tr>\n</thead>"
             (replace-regexp-in-string "\n\\([^<]\\|<[^t/]\\|</[^t]\\)" " \\1" html)))
    (should (= 1 (ox-wg21html-test-count "<thead>" html)))))

(ert-deftest cmptbl-code-row-is-not-a-header ()
  (let* ((org (replace-regexp-in-string
               "#\\+begin_cmptblcell before\n\\*Before\\*\n#\\+end_cmptblcell\n#\\+begin_cmptblcell after\n\\*After\\*\n#\\+end_cmptblcell\n"
               "" ox-wg21html-test-cmptbl))
         (html (ox-wg21html-test-export org)))
    (should-not (string-match-p "<thead>" html))
    (should (= 2 (ox-wg21html-test-count "<tr>" html)))))

(ert-deftest cmptbl-keeps-stray-content ()
  (let* ((org (replace-regexp-in-string
               "#\\+end_cmptbl\n" "A note about the table.\n#+end_cmptbl\n"
               ox-wg21html-test-cmptbl))
         (html (ox-wg21html-test-export org)))
    (should (string-match-p
             "<td class=\"cmptbl-note\" colspan=\"2\">.*A note about the table\\."
             (replace-regexp-in-string "\n" " " html)))))

(ert-deftest cmptbl-code-keeps-line-breaks ()
  (let ((html (ox-wg21html-test-export ox-wg21html-test-cmptbl)))
    (should (string-match-p
             "int</span> <span[^>]*>a</span> = 1;\n<span[^>]*>int</span>"
             html))))

(ert-deftest cmptbl-compact-form-has-caption-widths-and-general-columns ()
  (let ((html (ox-wg21html-test-export
               "#+caption: Three choices
#+attr_wg21: :columns 20 30 50
#+begin_cmptbl :headers \"Portable | POSIX | Native\"
#+begin_src C++
a();
#+end_src
#+begin_src C++
b();
#+end_src
#+begin_src C++
c();
#+end_src
#+end_cmptbl
")))
    (should (string-match-p "<caption>Three choices</caption>" html))
    (should (string-match-p "<col style=\"width: 20%\">" html))
    (should (string-match-p "<th>Portable</th><th>POSIX</th><th>Native</th>" html))
    (should (= 3 (ox-wg21html-test-count "<td class=\"cmptbl-" html)))))

(ert-deftest cmptbl-rejects-a-width-count-mismatch ()
  (should-error
   (ox-wg21html-test-export
    "#+attr_wg21: :columns 60 40
#+begin_cmptbl :headers \"A | B | C\"
#+begin_src C++
a();
#+end_src
#+end_cmptbl
")
   :type 'user-error))

(ert-deftest specgen-code-block-is-raw-code ()
  (let ((html (ox-wg21html-test-export
               "#+begin_codeblock\nT @\\exposidnc{value}@; // *not emphasis*\n#+end_codeblock\n")))
    (should (string-match-p
             (regexp-quote
              "<pre class=\"src src-C++\">T <var>value</var>; // *not emphasis*\n</pre>")
             html))
    (should-not (string-match-p "@\\\\exposidnc" html))))

(ert-deftest draft-escapes-in-raw-code-do-not-enable-mathjax ()
  (let ((html (ox-wg21html-test-export
               "#+begin_codeblock\nT @\\exposidnc{value}@;\n#+end_codeblock\n")))
    (should-not (string-match-p "MathJax" html))))

(ert-deftest specgen-code-block-references-are-links ()
  (let ((html (ox-wg21html-test-export
               "* Proposed\n:PROPERTIES:\n:CUSTOM_ID: proposed.clause\n:END:\n#+begin_codeblock\n// \\ref{proposed.clause}, \\ref{optional.general}\n#+end_codeblock\n")))
    (should (string-match-p
             (regexp-quote "<a href=\"#proposed.clause\">[proposed.clause]</a>")
             html))
    (should (string-match-p
             (regexp-quote
              "<a href=\"https://eel.is/c++draft/optional.general\">[optional.general]</a>")
             html))))

(ert-deftest ordinary-headlines-have-readable-stable-anchors ()
  (let ((html (ox-wg21html-test-export
               "#+OPTIONS: toc:2\n* Changes since R0\nSee [[*Relationship to =utf_transcoding_error= (P2728)][details]].\n** Relationship to =utf_transcoding_error= (P2728)\n")))
    (should (string-match-p "id=\"changes-since-r0\"" html))
    (should (string-match-p
             "id=\"relationship-to-utf_transcoding_error-p2728\"" html))
    (should (string-match-p
             "href=\"#relationship-to-utf_transcoding_error-p2728\">details</a>"
             html))
    (should (= 2 (ox-wg21html-test-count
                  "href=\"#changes-since-r0\"" html)))))

(ert-deftest explicit-and-repeated-headline-anchors-stay-unique ()
  (let ((html (ox-wg21html-test-export
               "* Generated\n:PROPERTIES:\n:CUSTOM_ID: repeated\n:END:\n* Repeated\n* Repeated\n")))
    (should (string-match-p "id=\"repeated\"" html))
    (should (string-match-p "id=\"repeated-2\"" html))
    (should (string-match-p "id=\"repeated-3\"" html))))

(ert-deftest wg21-table-columns-become-html-column-widths ()
  (let ((html (ox-wg21html-test-export
               "#+ATTR_WG21: :columns 18 36 36\n| A | B | C |\n|---+---+---|\n| a | b | c |\n")))
    (should (string-match-p "<table class=\"wg21-spec-table\"" html))
    (should (string-match-p "<colgroup>" html))
    (should-not (string-match-p "<col style=[^>]*group>" html))
    (should (= 1 (ox-wg21html-test-count "style=\"width: 18%\"" html)))
    (should (= 2 (ox-wg21html-test-count "style=\"width: 36%\"" html)))))

;; The comparison table last broke in the stylesheet, not the HTML: Org
;; 9.8 wraps code blocks in <code>, and wg21org.css made <code> nowrap,
;; so each block rendered as one long line that pushed the table past
;; the page.  Only a browser can see that.

(defconst ox-wg21html-test-root
  (expand-file-name ".." (file-name-directory (or (macroexp-file-name) buffer-file-name)))
  "The repository root.")

(defun ox-wg21html-test-browser ()
  "Return a headless-capable Chrome or Chromium, or nil."
  (seq-some #'executable-find '("google-chrome" "chromium" "chromium-browser")))

(defun ox-wg21html-test-render (body probe)
  "Render BODY with wg21org.css in a browser; return what PROBE computes.
PROBE is a JavaScript expression whose string value is returned."
  (let ((dir (make-temp-file "ox-wg21html-test" t)))
    (unwind-protect
        (let ((page (expand-file-name "page.html" dir)))
          (with-temp-file page
            (insert "<!DOCTYPE html><html><head><meta charset=\"utf-8\">"
                    "<link rel=\"stylesheet\" href=\"file://"
                    (expand-file-name "wg21org.css" ox-wg21html-test-root) "\">"
                    "</head><body>" body
                    "<script>document.title = String(" probe ");</script>"
                    "</body></html>"))
          (with-temp-buffer
            (call-process (ox-wg21html-test-browser) nil '(t nil) nil
                          "--headless=new" "--disable-gpu" "--no-sandbox"
                          "--allow-file-access-from-files" "--dump-dom"
                          (concat "file://" page))
            (goto-char (point-min))
            (and (re-search-forward "<title>\\([^<]*\\)</title>" nil t)
                 (match-string 1))))
      (delete-directory dir t))))

(defun ox-wg21html-test-render-page (html probe &optional dark)
  "Render the whole page HTML; return what the JavaScript PROBE computes.
With DARK, the browser reports a dark system colour scheme."
  (let ((dir (make-temp-file "ox-wg21html-test" t)))
    (unwind-protect
        (let ((page (expand-file-name "page.html" dir)))
          (with-temp-file page
            (insert (replace-regexp-in-string
                     "</body>"
                     (concat "<script>document.title = String(" probe ");</script></body>")
                     html t t)))
          (with-temp-buffer
            (call-process (ox-wg21html-test-browser) nil '(t nil) nil
                          "--headless=new" "--disable-gpu" "--no-sandbox"
                          (format "--blink-settings=preferredColorScheme=%d" (if dark 0 1))
                          "--dump-dom" (concat "file://" page))
            (goto-char (point-min))
            (and (re-search-forward "<title>\\([^<]*\\)</title>" nil t)
                 (match-string 1))))
      (delete-directory dir t))))

(ert-deftest cmptbl-renders-code-lines ()
  (skip-unless (ox-wg21html-test-browser))
  (should (equal (ox-wg21html-test-render
                  (ox-wg21html-test-export ox-wg21html-test-cmptbl)
                  "getComputedStyle(document.querySelector('pre.src code')).whiteSpace")
                 "pre")))

(ert-deftest cmptbl-fits-the-page ()
  (skip-unless (ox-wg21html-test-browser))
  (let ((long (replace-regexp-in-string
               "x();" (concat "x(" (make-string 300 ?a) ");")
               ox-wg21html-test-cmptbl)))
    (should (equal (ox-wg21html-test-render
                    (ox-wg21html-test-export long)
                    "document.documentElement.scrollWidth <= document.documentElement.clientWidth")
                   "true"))))

(ert-deftest wording-change-links ()
  (let ((html (ox-wg21html-test-export "a [[insert:][new]] b [[delete:][old]]")))
    (should (string-match-p "<ins>new</ins>" html))
    (should (string-match-p "<del>old</del>" html))))

(ert-deftest code-uses-face-classes ()
  (let* ((org-html-htmlize-output-type 'inline-css)
         (html (ox-wg21html-test-export
                "#+begin_src emacs-lisp\n(defun f () (list 1))\n#+end_src\n")))
    (should (string-match-p "class=\"org-keyword\"" html))
    (should (string-match-p "class=\"org-rainbow-delimiters-depth-1\"" html))
    (should-not (string-match-p "style=\"" html))))

(ert-deftest document-code-default-keywords-and-raw-override ()
  (let ((html (ox-wg21html-test-export
               "#+WG21_CODE_LANGUAGE: C++
#+WG21_CPP_KEYWORDS: inspect
#+begin_src
inspect (value) {}
#+end_src
#+begin_src text
inspect (plain)
#+end_src
")))
    (should (string-match-p "org-keyword[^>]*>inspect" html))
    (should (string-match-p
             "<pre class=\"src src-text\"><code>inspect (plain)" html))))

(ert-deftest git-remote-web-url ()
  (dolist (case '(("git@github.com:steve-downey/wg21org.git"
                   . "https://github.com/steve-downey/wg21org")
                  ("ssh://git@ceridwen.lan:22222/sdowney/wg21org.git"
                   . "https://ceridwen.lan/sdowney/wg21org")
                  ("https://mimir.lan/sdowney/wg21org"
                   . "https://mimir.lan/sdowney/wg21org")
                  ("https://gitlab.com/a/b.git/" . "https://gitlab.com/a/b")))
    (should (equal (wg21-git-https-url (car case)) (cdr case)))))

(ert-deftest git-blob-url-per-forge ()
  (should (equal (wg21-git-blob-url "https://github.com/o/r" "c0ffee" "d/x.org")
                 "https://github.com/o/r/blob/c0ffee/d/x.org"))
  (should (equal (wg21-git-blob-url "https://mimir.lan/o/r" "c0ffee" "d/x.org")
                 "https://mimir.lan/o/r/src/commit/c0ffee/d/x.org")))

(ert-deftest face-css-fixup ()
  (with-temp-buffer
    (insert "<style type=\"text/css\">\n    <!--\n      body {\n        color: #fff;\n      }\n"
            "      .org-keyword {\n        color: #f0f;\n      }\n"
            "      a {\n        color: inherit;\n      }\n    -->\n</style>\n")
    (wg21-html-face-css-fixup)
    (let ((once (buffer-string)))
      (should-not (string-match-p "<style\\|<!--\\|-->\\|</style>" once))
      (should (string-match-p "^ *pre\\.src {" once))
      (should (string-match-p "^ *pre\\.src a {" once))
      (should-not (string-match-p "^ *\\(body\\|a\\) {" once))
      (wg21-html-face-css-fixup)
      (should (equal once (buffer-string))))))

;;; Self-contained pages

(defun ox-wg21html-test-export-file (org &optional files)
  "Export ORG as a whole HTML page; see `wg21-test-export-file'."
  (wg21-test-export-file 'wg21-html org files))

(ert-deftest page-has-no-external-stylesheets ()
  (let ((html (ox-wg21html-test-export-file
               "#+TITLE: T\n#+HTML_HEAD: <link rel=\"stylesheet\" href=\"./extra.css\"/>\n* A\ntext\n"
               '(("extra.css" . ".extra-marker { color: red; }\n")))))
    (should-not (string-match-p "<link[^>]*stylesheet" html))
    (should (string-match-p "\\.extra-marker { color: red; }" html))
    (should (string-match-p "/\\* wg21org\\.css \\*/" html))
    (should-not (string-match-p "<script" html))))

(defconst ox-wg21html-test-png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
  "A one-pixel PNG.")

(ert-deftest page-embeds-local-images ()
  (let ((html (ox-wg21html-test-export-file
               "#+TITLE: T\n* A\n[[file:images/dot.png]]\n"
               `(("images/dot.png" . ,ox-wg21html-test-png)))))
    (should-not (string-match-p "<img[^>]*src=\"images/" html))
    (should (string-match "<img[^>]*src=\"data:image/png;base64,\\([^\"]*\\)\"" html))
    (should (equal (base64-decode-string (match-string 1 html)) ox-wg21html-test-png))))

(ert-deftest page-leaves-missing-images-for-check ()
  (let ((html (ox-wg21html-test-export-file "#+TITLE: T\n* A\n[[file:missing.png]]\n")))
    (should (string-match-p "<img[^>]*src=\"missing\\.png\"" html))))

(ert-deftest empty-bibliography-is-dropped ()
  (let ((html (ox-wg21html-test-export-file
               (wg21-test-paper-with-references "No citations here.")
               (wg21-test-bibliography-files))))
    (should-not (string-match-p ">References<" html))
    (should (string-match-p ">Intro<" html))))

(ert-deftest bibliography-is-kept-when-cited ()
  (let ((html (ox-wg21html-test-export-file
               (wg21-test-paper-with-references "See [cite:@rfc3514].")
               (wg21-test-bibliography-files))))
    (should (string-match-p ">References<" html))
    (should (string-match-p "class=\"csl-entry\"" html))))

(ert-deftest diff-toggle-only-with-deleted-text ()
  (should (string-match-p
           "id=\"wg21-hide-deleted\""
           (ox-wg21html-test-export-file "#+TITLE: T\n* A\nMake it [[delete:][fail]].\n")))
  (should (string-match-p
           "id=\"wg21-hide-deleted\""
           (ox-wg21html-test-export-file "#+TITLE: T\n* A\n#+begin_removedblock\nold\n#+end_removedblock\n")))
  (should-not (string-match-p
               "id=\"wg21-hide-deleted\""
               (ox-wg21html-test-export-file "#+TITLE: T\n* A\nNothing removed.\n"))))

(ert-deftest diff-toggle-hides-deleted-text ()
  "The toggle is CSS alone: checked, deletions vanish and insertions are plain."
  (skip-unless (ox-wg21html-test-browser))
  (let ((page (lambda (checked)
                (concat "<input type=\"checkbox\" id=\"wg21-hide-deleted\"" checked ">"
                        "<p>A <del id=\"d\">b</del> <ins id=\"i\">c</ins></p>")))
        (probe (concat "getComputedStyle(document.getElementById('d')).display + ' '"
                       " + getComputedStyle(document.getElementById('i')).textDecorationLine")))
    (should (equal (ox-wg21html-test-render (funcall page "") probe) "inline underline"))
    (should (equal (ox-wg21html-test-render (funcall page " checked") probe) "none none"))))

(ert-deftest cite-urls-in-entries ()
  (should (equal (wg21-cite-urls "Reis. “P3589R1.” https://wg21.link/p3589r1; WG21.")
                 '("https://wg21.link/p3589r1")))
  (should (equal (wg21-cite-urls "RFC Editor. \\url{https://doi.org/10.17487/RFC3514}.")
                 '("https://doi.org/10.17487/RFC3514"))))

(ert-deftest cite-single-urls-only ()
  (let ((urls (wg21-cite-single-urls '(("1" "https://a" "https://a")
                                       ("2" "https://a" "https://b")
                                       ("3")))))
    (should (equal (gethash "1" urls) "https://a"))
    (should-not (gethash "2" urls))
    (should-not (gethash "3" urls))))

(ert-deftest citation-links-to-the-paper ()
  (let ((html (ox-wg21html-test-export-file
               (wg21-test-paper-with-references "See [cite:@rfc3514].")
               (wg21-test-bibliography-files))))
    (should (string-match-p
             "<a href=\"https://doi.org/10.17487/RFC3514\" title=\"[^\"]*Security Flag[^\"]*\">"
             html))
    (should-not (string-match-p "href=\"#citeproc_bib_item" html))))

(ert-deftest title-citation-uses-the-default-wg21-index ()
  (let ((html (ox-wg21html-test-export "See [[cite-title:P2996R8]].")))
    (should (string-match-p
             "href=\"https://wg21.link/p2996r8\">\\[P2996R8\\] (.*Reflection"
             html))
    (should (string-match-p ">References<" html))
    (should (string-match-p "P2996R8: Reflection" html))))

(ert-deftest wg21-citation-needs-no-bibliography-boilerplate ()
  (let ((html (ox-wg21html-test-export-file
               "#+TITLE: T\n* Intro\nSee [cite:@P2996R13].\n")))
    (should (string-match-p ">References<" html))
    (should (string-match-p "Reflection for C.*26" html))))

(ert-deftest recognized-paper-number-has-latest-and-status-links ()
  (let ((html (ox-wg21html-test-export-file
               "#+TITLE: T\n#+DOCNUMBER: D4246R1\n* A\n")))
    (should (string-match-p "https://wg21.link/P4246\">Latest" html))
    (should (string-match-p "https://wg21.link/P4246/status\">Status" html))))

(ert-deftest wording-code-is-not-highlighted ()
  (let ((html (ox-wg21html-test-export-file wg21-test-wording-paper)))
    (should (string-match-p "<span class=\"org-function-name\">outside</span>" html))
    (should (string-match-p "<code>int inside(<ins>int</ins>); // plain\n</code>" html))))

(ert-deftest generated-wording-root-and-paragraphs ()
  (let ((html (ox-wg21html-test-export
               "* Clause\n:PROPERTIES:\n:WG21_WORDING: t\n:END:\n#+begin_pnum\n/Effects/: text.\n#+end_pnum\n")))
    (should (string-match-p "class=\"[^\"]*wg21-wording" html))
    (should (string-match-p "class=\"pnum\"" html))))

(ert-deftest added-wording-root-carries-an-html-addition-scope ()
  (let ((html (ox-wg21html-test-export
               "* Clause\n:PROPERTIES:\n:WG21_WORDING: t\n:WG21_CHANGE: add\n:END:\nText.\n")))
    (should (string-match-p "class=\"[^\"]*wg21-addition" html))))

(ert-deftest explicit-paragraph-numbers-survive-html-export ()
  (let ((html (ox-wg21html-test-export
               "#+begin_pnum x+2\nAdded wording.\n#+end_pnum\n")))
    (should (string-match-p
             (regexp-quote "class=\"pnum pnum-explicit\" data-pnum=\"x+2\"") html))))

(ert-deftest note-like-blocks-carry-number-and-audience ()
  (let ((html (ox-wg21html-test-export
               "#+begin_note :number 5\nN.\n#+end_note\n#+begin_draftnote :audience LEWG\nD.\n#+end_draftnote\n")))
    (should (string-match-p "class=\"wg21-note\" style=\"counter-set: wg21-note 4\"" html))
    (should (string-match-p "class=\"wg21-draftnote\" data-audience=\"LEWG\"" html))))

(ert-deftest stable-name-links-choose-local-or-draft-targets ()
  (let ((html (ox-wg21html-test-export
               "* Proposed\n:PROPERTIES:\n:CUSTOM_ID: proposed.clause\n:END:\n[[sref:proposed.clause]] [[sref:basic.life/2.1]]\n")))
    (should (string-match-p (regexp-quote "href=\"#proposed.clause\">[proposed.clause]</a>") html))
    (should (string-match-p
             (regexp-quote "href=\"https://eel.is/c++draft/basic.life#2.1\">[basic.life]/2.1</a>") html))))

(ert-deftest raw-wording-code-supports-edits-and-grammar ()
  (let ((html (ox-wg21html-test-export
               "#+begin_codeblock\nint @\\added{x}@ = @\\removed{y}@;\n#+end_codeblock\n#+begin_grammar\n@\\grammarterm{statement}@ : @\\terminal{break}@\n#+end_grammar\n")))
    (should (string-match-p "int <ins>x</ins> = <del>y</del>;" html))
    (should (string-match-p "<pre class=\"grammar\"><var>statement</var> : <code>break</code>" html))))

(ert-deftest abstract-comes-before-contents ()
  (let ((html (ox-wg21html-test-export-file (wg21-test-paper-with-abstract)
                                            (wg21-test-bibliography-files))))
    (should (< (string-search "class=\"abstract\"" html)
               (string-search "id=\"toc\"" html)))
    (should (= 1 (length (wg21-cite-matches "class=\"abstract\"" html))))
    (should (string-match-p "<div class=\"abstract\"[^>]*>\\(?:.\\|\n\\)*?<a href=\"https://doi.org/10.17487/RFC3514\"" html))))

(ert-deftest scope-css-prefixes-every-selector ()
  (should (equal (wg21-html--scope-css "pre.src, .org-a { color: red; }\n.org-b {x: y;}" "S")
                 "S pre.src, S .org-a { color: red; }\nS .org-b {x: y;}")))

(ert-deftest theme-switch-sets-page-and-opposite-code ()
  "Light page, dark code; dark page, light code; the switch beats the system."
  (skip-unless (ox-wg21html-test-browser))
  (let* ((html (ox-wg21html-test-export-file
                "#+TITLE: T\n* A\n#+begin_src C++\nint a;\n#+end_src\n"))
         (pick (lambda (mode)
                 (replace-regexp-in-string
                  (format "id=\"wg21-mode-%s\">" mode)
                  (format "id=\"wg21-mode-%s\" checked>" mode)
                  (replace-regexp-in-string " checked> System" "> System" html t t)
                  t t)))
         (probe (concat "getComputedStyle(document.body).backgroundColor + ' '"
                        " + getComputedStyle(document.querySelector('pre.src')).backgroundColor"))
         (light "rgb(252, 252, 250) rgb(13, 14, 28)")
         (dark "rgb(22, 23, 29) rgb(251, 247, 240)"))
    (should (string-match-p "id=\"wg21-mode-system\" checked" html))
    (should (equal (ox-wg21html-test-render-page html probe) light))
    (should (equal (ox-wg21html-test-render-page html probe t) dark))
    (should (equal (ox-wg21html-test-render-page (funcall pick "dark") probe) dark))
    (should (equal (ox-wg21html-test-render-page (funcall pick "light") probe t) light))))

(ert-deftest page-title-is-plain-text ()
  (let ((html (ox-wg21html-test-export-file "#+TITLE: A view of ~view::maybe~\n* A\n")))
    (should (string-match-p "<title>A view of view::maybe</title>" html))))

(ert-deftest headline-links-to-source-line ()
  (let ((html (ox-wg21html-test-export-file
               "#+TITLE: T\n\n* First\ntext\n** Second\nmore\n* First again\n")))
    (should (string-match-p "blob/[0-9a-f]+/paper\\.org\\?plain=1#L3\"[^>]*>source</a></h2>" html))
    (should (string-match-p "#L5\"[^>]*>source</a></h3>" html))
    (should (string-match-p "#L7\"[^>]*>source</a></h2>" html))))

(ert-deftest git-blob-url-with-line ()
  (should (equal (wg21-git-blob-url "https://github.com/o/r" "c0ffee" "x.org" 12)
                 "https://github.com/o/r/blob/c0ffee/x.org?plain=1#L12"))
  (should (equal (wg21-git-blob-url "https://mimir.lan/o/r" "c0ffee" "x.org" 12)
                 "https://mimir.lan/o/r/src/commit/c0ffee/x.org?display=source#L12")))

(ert-deftest face-css-trimmed-to-used-classes ()
  (let* ((css "pre.src {\n color: #000;\n}\n.org-keyword {\n /* k */ color: #f0f;\n}\n.org-string {\n color: #0f0;\n}\n")
         (classes (make-hash-table :test #'equal)))
    (puthash "org-keyword" t classes)
    (let ((trimmed (wg21-html--trim-css css classes)))
      (should (string-match-p "pre\\.src" trimmed))
      (should (string-match-p "\\.org-keyword" trimmed))
      (should-not (string-match-p "\\.org-string" trimmed)))
    (should (equal (wg21-html--trim-css (concat "@media print {}\n" css) classes)
                   (concat "@media print {}\n" css)))))

(ert-deftest multi-line-math-is-mathml ()
  (skip-unless (executable-find "pandoc"))
  (should (string-match-p "\\`<math display=\"block\""
                          (wg21-html--mathml "\\[\n\\int_0^\\infty e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}\n\\]"))))

(ert-deftest math-is-mathml ()
  (skip-unless (executable-find "pandoc"))
  (let ((html (ox-wg21html-test-export-file "#+TITLE: T\n* A\nIf $x^2 = y$ then\n")))
    (should (string-match-p "<math display=\"inline\"" html))
    (should-not (string-match-p "MathJax" html))))

;;; ox-wg21html-test.el ends here
