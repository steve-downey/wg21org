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

(ert-deftest latex-headlines-and-links-use-readable-stable-labels ()
  (let ((latex (ox-wg21latex-test-export
                "* Changes since R0\nSee [[*Changes since R0][above]].\n")))
    (should (string-match-p "\\\\label{sec:changes-since-r0}" latex))
    (should (string-match-p
             "\\\\hyperref\\[sec:changes-since-r0\\]{above}" latex))))

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

(ert-deftest latex-cmptbl-compact-form-has-caption-widths-and-columns ()
  (let ((latex (ox-wg21latex-test-export
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
    (should (string-match-p "\\\\begin{wgcmptbl}{3}" latex))
    (should (string-match-p "\\\\caption{Three choices}" latex))
    (should (string-match-p "linewidth/100\\*20" latex))
    (should (= 3 (ox-wg21latex-test-count "\\\\begin{wgcmptblcell}" latex)))))

(ert-deftest latex-document-code-default-and-raw-override ()
  (let ((latex (ox-wg21latex-test-export
                "#+WG21_CODE_LANGUAGE: C++
#+begin_src
int highlighted;
#+end_src
#+begin_src text
int plain;
#+end_src
")))
    (should (string-match-p "highlighted" latex))
    (should (string-match-p "\\\\begin{verbatim}\nint plain;" latex))))

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

(ert-deftest latex-generated-wording-root-and-paragraphs ()
  (let ((latex (ox-wg21latex-test-export
                "* Clause\n:PROPERTIES:\n:UNNUMBERED: t\n:WG21_WORDING: t\n:END:\n#+begin_pnum\n/Effects/: text.\n#+end_pnum\n")))
    (should (string-match-p "\\\\begin{wgwording}" latex))
    (should (string-match-p "\\\\chapter\\*{Clause}" latex))
    (should-not (string-match-p "\\\\chapter{Clause}" latex))
    (should (string-match-p (regexp-quote "\\wgexplicitpnum{1}") latex))
    (should (string-match-p "\\\\end{wgwording}" latex))))

(ert-deftest latex-added-wording-root-wraps-the-subtree ()
  (let ((latex (ox-wg21latex-test-export
                "* Clause\n:PROPERTIES:\n:WG21_WORDING: t\n:WG21_CHANGE: add\n:END:\nText.\n")))
    (should (< (string-search "\\begin{addedblock}" latex)
               (string-search "\\begin{wgwording}" latex)))
    (should (< (string-search "\\end{wgwording}" latex)
               (string-search "\\end{addedblock}" latex)))))

(ert-deftest latex-explicit-paragraph-number ()
  (let ((latex (ox-wg21latex-test-export
                "#+begin_pnum x+2\nAdded wording.\n#+end_pnum\n")))
    (should (string-match-p (regexp-quote "\\wgexplicitpnum{x+2}") latex))))

(ert-deftest latex-note-like-blocks ()
  (let ((latex (ox-wg21latex-test-export
                "#+begin_note :number 5\nN.\n#+end_note\n#+begin_draftnote :audience LEWG\nD.\n#+end_draftnote\n")))
    (should (string-match-p "\\\\wgsetcounterifdefined{note}{4}" latex))
    (should (string-match-p "\\\\begin{wgblock}{note}" latex))
    (should (string-match-p "\\\\wgdraftnote\\[LEWG\\]" latex))))

(ert-deftest latex-stable-name-links-choose-local-or-draft-targets ()
  (let ((latex (ox-wg21latex-test-export
                "* Proposed\n:PROPERTIES:\n:CUSTOM_ID: proposed.clause\n:END:\n#+latex: \\\\label{proposed.clause}\n[[sref:proposed.clause]] [[sref:basic.life/2.1]]\n")))
    (should (string-match-p (regexp-quote "\\hyperref[proposed.clause]{[proposed.clause]}") latex))
    (should (string-match-p
             (regexp-quote "\\href{https://eel.is/c++draft/basic.life#2.1}{[basic.life]/2.1}") latex))))

(ert-deftest latex-grammar-is-a-draft-grammar-block ()
  (let ((latex (ox-wg21latex-test-export
                "#+begin_grammar\n@\\grammarterm{statement}@\n#+end_grammar\n")))
    (should (string-match-p "\\\\begin{wgblock}{ncbnf}" latex))
    (should (string-match-p (regexp-quote "@\\grammarterm{statement}@") latex))))

(ert-deftest latex-specgen-code-block-is-raw ()
  (let ((latex (ox-wg21latex-test-export
                "#+begin_codeblock\nT @\\exposidnc{value}@; // *not emphasis*, \\ref{optional.general}\n#+end_codeblock\n")))
    (should (string-match-p "\\\\begin{codeblock}" latex))
    (should (string-match-p
             "T @\\\\exposidnc{value}@; // \\*not emphasis\\*, \\[optional.general\\]" latex))
    (should-not (string-match-p "\\\\ref{" latex))
    (should-not (string-match-p "\\\\begin{wgblock}{codeblock}" latex))))

(ert-deftest latex-wg21-table-columns-become-longtable-widths ()
  (let ((latex (ox-wg21latex-test-export
                "#+ATTR_WG21: :columns 18 36 36\n| A | B | C |\n|---+---+---|\n| a | b | c |\n")))
    (should (string-match-p "\\\\begin{longtable}" latex))
    (should (string-match-p
             (regexp-quote "@{}p{.18\\linewidth}p{.36\\linewidth}p{.36\\linewidth}@{}")
             latex))))

(ert-deftest latex-wording-change-links ()
  (let ((latex (ox-wg21latex-test-export "a [[insert:][new]] b [[delete:][old]]")))
    (should (string-match-p "\\\\added{new}" latex))
    (should (string-match-p "\\\\removed{old}" latex))))

(ert-deftest latex-substitution-and-mark-links ()
  (let ((latex (ox-wg21latex-test-export
                "Use [[replace:%2Fnew%2F][old /text/]] and [[mark:][important *text*]].\n")))
    (should (string-match-p
             (regexp-quote "\\removed{old \\emph{text}}\\added{\\emph{new}}") latex))
    (should (string-match-p
             (regexp-quote "\\wgmark{important \\textbf{text}}") latex))))

(ert-deftest latex-wording-lists-can-be-paragraph-numbered ()
  (let ((latex (ox-wg21latex-test-export
                (concat "#+begin_wording :pnums lists\n"
                        "1. First.\n2. Second.\n   - Nested.\n3. Third.\n"
                        "#+end_wording\n"))))
    (dolist (label '("1" "2" "2.1" "3"))
      (should (string-match-p
               (regexp-quote (format "\\wgexplicitpnum{%s}" label)) latex)))
    (should-not (string-match-p "\\\\begin{enumerate}" latex))))

(ert-deftest latex-raw-code-markup-nests-without-nested-escape-delimiters ()
  (let ((latex (ox-wg21latex-test-export
                (concat "#+begin_codeblock\n"
                        "@\\added{T{1}, @\\emph{term}@}@\n"
                        "#+end_codeblock\n"))))
    (should (string-match-p
             (regexp-quote "@\\added{T{1}, \\textit{term}}@") latex))))

(ert-deftest latex-title-block ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                "#+TITLE: T\n#+DOCNUMBER: P9999R0\n#+EMAIL: a@b.c\n\n* First\ntext\n")))
    (should (string-match-p "\\\\wgmetalabel{Document \\\\#:} & \\\\wgmetavalue{P9999R0}" latex))
    (should (string-match-p "\\\\href{https://github.com/o/r/blob/[0-9a-f]+/paper\\.org}{paper\\.org}" latex))
    ;; A printed page has no use for a link beside each section.
    (should-not (string-match-p "wgsourcelink" latex))
    ;; The metadata comes from the exporter, not from macros a paper may lack.
    (should-not (string-match-p "\\\\docnumber{" latex))
    ;; memoir's starred form would print a * under any other class.
    (should (string-match-p "^\\\\wgtableofcontents$" latex))))

(ert-deftest latex-empty-bibliography-is-dropped ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                (wg21-test-paper-with-references "No citations here.")
                (wg21-test-bibliography-files))))
    (should-not (string-match-p "\\(?:chapter\\|section\\){References}" latex))
    (should (string-match-p "\\(?:chapter\\|section\\){Intro}" latex))))

(ert-deftest latex-bibliography-is-kept-when-cited ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                (wg21-test-paper-with-references "See [cite:@rfc3514].")
                (wg21-test-bibliography-files))))
    (should (string-match-p "\\(?:chapter\\|section\\){References}" latex))
    (should (string-match-p "\\\\cslbibitem{1}" latex))))

(ert-deftest latex-citation-links-to-the-paper ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                (wg21-test-paper-with-references "See [cite:@rfc3514].")
                (wg21-test-bibliography-files))))
    (should (string-match-p "\\\\href{https://doi.org/10.17487/RFC3514}{" latex))
    (should-not (string-match-p "^[^%]*\\\\cslcitation{[0-9]" latex))))

(ert-deftest latex-prolog-is-common-by-default ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                "#+TITLE: T\n#+LATEX_HEADER: \\include{common.tex}\n* A\n")))
    (should (string-match-p "\\\\documentclass\\[[^]]*article\\]{memoir}" latex))
    (should (string-match-p "^%% ---- common\\.tex$" latex))
    (should (string-match-p "^%% ---- stdtex/macros$" latex))
    ;; Inlined, so nothing is read from beside the paper.
    (should-not (string-match-p "^[ \t]*\\\\\\(?:input\\|include\\){\\(?:common\\|stdtex/\\)" latex))))

(ert-deftest latex-prolog-can-be-overridden ()
  (let ((latex (wg21-test-export-file
                'wg21-latex
                "#+TITLE: T\n#+WG21_LATEX_PROLOG: none\n* A\n")))
    (should-not (string-match-p "^%% ---- common\\.tex$" latex))))

(ert-deftest latex-wording-code-is-codeblock ()
  (let ((latex (wg21-test-export-file 'wg21-latex wg21-test-wording-paper)))
    (should (string-match-p
             "\\\\begin{codeblock}\nint inside(@\\\\added{int}@); // plain\n\\\\end{codeblock}"
             latex))
    (should (string-match-p "\\\\begin{Code}\n\\\\begin{Verbatim}.*\n.*outside" latex))))

(ert-deftest latex-wording-paper-compiles ()
  (skip-unless (and (executable-find "latexmk") (executable-find "lualatex")))
  (let* ((latex (wg21-test-export-file
                 'wg21-latex
                 (concat "#+LATEX_COMPILER: lualatex\n" wg21-test-wording-paper)))
         (dir (make-temp-file "ox-wg21latex-test" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (with-temp-file "paper.tex" (insert latex))
          (should (eql 0 (call-process "latexmk" nil nil nil
                                       "-lualatex" "-interaction=nonstopmode"
                                       "-halt-on-error" "paper.tex"))))
      (delete-directory dir t))))

(ert-deftest latex-title-page-layout ()
  "Title and authors centred, document block flush right, as the working draft's papers."
  (let ((latex (wg21-test-export-file
                'wg21-latex
                "#+TITLE: T\n#+AUTHOR: Ann One, Bob Two\n#+EMAIL: ann@x.org, bob@y.org\n* A\n")))
    (should (string-match-p "\\\\begin{center}\n{\\\\LARGE T\\\\par}" latex))
    (should (string-match-p "^Ann One {\\\\small\\\\textless\\\\href{mailto:ann@x\\.org}" latex))
    (should (string-match-p "^Bob Two {\\\\small\\\\textless\\\\href{mailto:bob@y\\.org}" latex))
    (should (string-match-p "\\\\begin{flushright}\n\\\\begin{tabular}" latex))
    (should (string-match-p "\\\\wgmetalabel{Project:} & \\\\wgmetavalue{Programming Language C\\+\\+}" latex))))

(ert-deftest latex-abstract-comes-before-contents ()
  (let ((latex (wg21-test-export-file 'wg21-latex (wg21-test-paper-with-abstract)
                                      (wg21-test-bibliography-files))))
    (should (< (string-search "\\begin{wgblock}{abstract}" latex)
               (string-search "\n\\wgtableofcontents" latex)))
    (should (= 1 (length (wg21-cite-matches "begin{wgblock}{abstract}" latex))))
    (should (string-match-p "We build on (\\\\href{https://doi.org/10.17487/RFC3514}" latex))))

(ert-deftest latex-paper-compiles ()
  "A paper with no preamble of its own compiles, comparison table and all."
  (skip-unless (and (executable-find "latexmk") (executable-find "lualatex")))
  (let* ((latex (wg21-test-export-file
                 'wg21-latex
                 (concat "#+TITLE: T\n#+LATEX_COMPILER: lualatex\n\n* A\n"
                         "[[mark:][Marked *text*]] and [[replace:new][old]].\n"
                         "#+begin_pnum x+1\nAdded.\n#+end_pnum\n"
                         "#+begin_note :number 5\nA note.\n#+end_note\n"
                         "#+begin_draftnote :audience LEWG\nReview.\n#+end_draftnote\n"
                         "#+begin_grammar\n@\\grammarterm{statement}@\n#+end_grammar\n"
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
