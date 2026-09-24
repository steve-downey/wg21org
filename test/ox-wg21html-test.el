;;; ox-wg21html-test.el --- Tests for the WG21 HTML exporter  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
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
  "Export ORG as a whole page from a file in a scratch git repository.
FILES is an alist of (NAME . CONTENTS) written beside it first.  ORG is
committed, so source links resolve.  Return the HTML."
  (let* ((dir (make-temp-file "ox-wg21html-test" t))
         (default-directory (file-name-as-directory dir))
         (file (expand-file-name "paper.org" dir)))
    (unwind-protect
        (progn
          (dolist (extra files)
            (with-temp-file (expand-file-name (car extra) dir) (insert (cdr extra))))
          (with-temp-file file (insert org))
          (dolist (args '(("init" "-q") ("add" ".")
                          ("-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "t")
                          ("remote" "add" "origin" "git@github.com:o/r.git")))
            (apply #'call-process "git" nil nil nil args))
          (let ((buffer (find-file-noselect file))
                (org-export-use-babel nil))
            (unwind-protect
                (with-current-buffer buffer (org-export-as 'wg21-html))
              (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest page-has-no-external-stylesheets ()
  (let ((html (ox-wg21html-test-export-file
               "#+TITLE: T\n#+HTML_HEAD: <link rel=\"stylesheet\" href=\"./extra.css\"/>\n* A\ntext\n"
               '(("extra.css" . ".extra-marker { color: red; }\n")))))
    (should-not (string-match-p "<link[^>]*stylesheet" html))
    (should (string-match-p "\\.extra-marker { color: red; }" html))
    (should (string-match-p "/\\* wg21org\\.css \\*/" html))
    (should-not (string-match-p "<script" html))))

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

(ert-deftest math-is-mathml ()
  (skip-unless (executable-find "pandoc"))
  (let ((html (ox-wg21html-test-export-file "#+TITLE: T\n* A\nIf $x^2 = y$ then\n")))
    (should (string-match-p "<math display=\"inline\"" html))
    (should-not (string-match-p "MathJax" html))))

;;; ox-wg21html-test.el ends here
