;;; ox-wg21latex-test.el --- Tests for the WG21 LaTeX exporter  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'wg21-test-support
         (expand-file-name "wg21-test-support"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'ox-wg21latex
         (expand-file-name "../ox-wg21latex"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

(defun ox-wg21latex-test-export (org)
  "Export the Org text ORG with the WG21 LaTeX backend, body only."
  (let ((org-export-use-babel nil))
    (org-export-string-as org 'wg21-latex t)))

(defun ox-wg21latex-test-count (regexp string)
  "Count the matches of REGEXP in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defconst ox-wg21latex-test-cmptbl "\
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
#+end_src
#+end_cmptblcell
#+begin_cmptblcell after
#+begin_src C++
auto a = f(1);
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

(ert-deftest latex-cmptbl-is-a-table ()
  (let ((latex (ox-wg21latex-test-export ox-wg21latex-test-cmptbl)))
    (should (string-match-p "\\\\begin{wgcmptbl}" latex))
    (should-not (string-match-p "{cmptblcell}" latex))
    (should (= 4 (ox-wg21latex-test-count "\\\\begin{wgcmptblcell}" latex)))
    (should (= 1 (ox-wg21latex-test-count "\\\\midrule\n\\\\begin{wgcmptblcell}" latex)))))

(ert-deftest latex-cmptbl-header-row ()
  (let ((latex (ox-wg21latex-test-export ox-wg21latex-test-cmptbl)))
    (should (string-match-p
             "\\\\multicolumn{1}{@{}c}{\\\\textbf{Before}} & \\\\multicolumn{1}{c@{}}{\\\\textbf{After}} \\\\\\\\\n\\\\midrule\n\\\\endhead"
             latex))))

(ert-deftest latex-cmptbl-code-row-is-not-a-header ()
  (let* ((org (replace-regexp-in-string
               "#\\+begin_cmptblcell before\n\\*Before\\*\n#\\+end_cmptblcell\n#\\+begin_cmptblcell after\n\\*After\\*\n#\\+end_cmptblcell\n"
               "" ox-wg21latex-test-cmptbl))
         (latex (ox-wg21latex-test-export org)))
    (should-not (string-match-p "\\\\endhead" latex))
    (should (= 4 (ox-wg21latex-test-count "\\\\begin{wgcmptblcell}" latex)))))

(ert-deftest latex-block-names-ignore-case ()
  (let ((latex (ox-wg21latex-test-export "#+BEGIN_ABSTRACT\nx\n#+END_ABSTRACT\n")))
    (should (string-match-p "\\\\begin{wgblock}{abstract}" latex))))

(ert-deftest latex-footnotes-work-with-either-footnote ()
  "common.tex turns \\footnote into an environment; \\wgfootnote copes."
  (let ((latex (ox-wg21latex-test-export "Text[fn:: A note.] more.\n")))
    (should (string-match-p "\\\\wgfootnote{A note\\.}" latex))
    (should-not (string-match-p "\\\\footnote{" latex))))

(ert-deftest latex-special-block-is-guarded ()
  "A block whose environment the class may lack cannot stop the build."
  (let ((latex (ox-wg21latex-test-export "#+begin_tip\nx\n#+end_tip\n")))
    (should (string-match-p "\\\\begin{wgblock}{tip}\nx\n\\\\end{wgblock}" latex))))

(ert-deftest latex-wording-change-links ()
  (let ((latex (ox-wg21latex-test-export "a [[insert:][new]] b [[delete:][old]]")))
    (should (string-match-p "\\\\added{new}" latex))
    (should (string-match-p "\\\\removed{old}" latex))))

(ert-deftest latex-title-block-and-source-links ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                "#+TITLE: T\n#+DOCNUMBER: P9999R0\n#+EMAIL: a@b.c\n\n* First\ntext\n")))
    (should (string-match-p "\\\\wgmetalabel{Document \\\\#:} & \\\\wgmetavalue{P9999R0}" latex))
    (should (string-match-p "\\\\href{https://github.com/o/r/blob/[0-9a-f]+/paper\\.org}{paper\\.org}" latex))
    (should (string-match-p "\\\\wgsourcelink{https://github.com/o/r/blob/[0-9a-f]+/paper\\.org\\?plain=1\\\\#L5}" latex))
    ;; The metadata comes from the exporter, not from macros a paper may lack.
    (should-not (string-match-p "\\\\docnumber{" latex))
    ;; memoir's starred form would print a * under any other class.
    (should (string-match-p "^\\\\wgtableofcontents$" latex))))

(ert-deftest latex-empty-bibliography-is-dropped ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                (wg21-test-paper-with-references "No citations here.")
                (wg21-test-bibliography-files))))
    (should-not (string-match-p "section{References}" latex))
    (should (string-match-p "section{Intro}" latex))))

(ert-deftest latex-bibliography-is-kept-when-cited ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                (wg21-test-paper-with-references "See [cite:@rfc3514].")
                (wg21-test-bibliography-files))))
    (should (string-match-p "section{References}" latex))
    (should (string-match-p "\\\\cslbibitem{1}" latex))))

(ert-deftest latex-paper-compiles ()
  "A paper with no preamble of its own compiles, comparison table and all."
  (skip-unless (and (executable-find "latexmk") (executable-find "lualatex")))
  (let* ((latex (wg21-test-export-file
                 'wg21-latex
                 (concat "#+TITLE: T\n#+LATEX_COMPILER: lualatex\n\n* A\n"
                         ox-wg21latex-test-cmptbl)))
         (dir (make-temp-file "ox-wg21latex-test" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (with-temp-file "paper.tex" (insert latex))
          (should (eql 0 (call-process "latexmk" nil nil nil
                                       "-lualatex" "-interaction=nonstopmode"
                                       "-halt-on-error" "paper.tex")))
          (should (file-exists-p "paper.pdf")))
      (delete-directory dir t))))

;;; ox-wg21latex-test.el ends here
