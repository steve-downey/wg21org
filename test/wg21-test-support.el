;;; wg21-test-support.el --- Fixtures for the WG21 exporter tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Exporting a whole paper needs a file in a git repository, so that
;; the git metadata and source links have something to point at.

;;; Code:

(require 'ox)

(defun wg21-test-export-file (backend org &optional files)
  "Export ORG with BACKEND from a file in a scratch git repository.
FILES is an alist of (NAME . CONTENTS) written beside it first.  ORG
is committed, with a GitHub origin, so source links resolve.  Return
the exported text."
  (let* ((dir (make-temp-file "wg21-test" t))
         (default-directory (file-name-as-directory dir))
         (file (expand-file-name "paper.org" dir)))
    (unwind-protect
        (progn
          ;; Written byte for byte, so a unibyte string can be an image.
          (let ((coding-system-for-write 'no-conversion))
            (dolist (extra files)
              (let ((path (expand-file-name (car extra) dir)))
                (make-directory (file-name-directory path) t)
                (with-temp-file path (insert (cdr extra))))))
          (with-temp-file file (insert org))
          (dolist (args '(("init" "-q") ("add" ".")
                          ("-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "t")
                          ("remote" "add" "origin" "git@github.com:o/r.git")))
            (apply #'call-process "git" nil nil nil args))
          (let ((buffer (find-file-noselect file))
                (org-export-use-babel nil))
            (unwind-protect
                (with-current-buffer buffer (org-export-as backend))
              (kill-buffer buffer))))
      (delete-directory dir t))))

(defconst wg21-test-directory
  (expand-file-name ".." (file-name-directory (or (macroexp-file-name) buffer-file-name)))
  "The repository root.")

(defun wg21-test-bibliography-files ()
  "Files for a paper with a bibliography: refs.bib and a CSL style.
refs.bib holds rfc3514, an RFC with a single DOI URL."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (with-temp-buffer
                    (insert-file-contents (expand-file-name (cdr pair) wg21-test-directory))
                    (buffer-string))))
          '(("refs.bib" . "rfc3514.bib")
            ("style.csl" . "chicago-author-date.csl"))))

(defun wg21-test-paper-with-references (body)
  "Return a paper with BODY, a bibliography, and a References heading."
  (concat "#+TITLE: T\n#+BIBLIOGRAPHY: refs.bib\n"
          "* Intro\n" body "\n"
          "* References\n#+CITE_EXPORT: csl style.csl\n#+PRINT_BIBLIOGRAPHY:\n"))

(defconst wg21-test-wording-paper "\
#+TITLE: T
* Motivation
#+begin_src C++
int outside(); // highlighted
#+end_src
* Wording
#+begin_wording
Change the synopsis:
#+begin_src C++
int inside(@\\added{int}@); // plain
#+end_src
#+end_wording
"
  "A paper with code both outside and inside its wording.")

(defun wg21-test-paper-with-abstract ()
  "Return a paper whose abstract cites a reference, with a table of contents."
  (concat "#+TITLE: T\n#+OPTIONS: toc:t\n#+BIBLIOGRAPHY: refs.bib\n"
          "#+begin_abstract\nWe build on [cite:@rfc3514].\n#+end_abstract\n"
          "* Intro\ntext\n"
          "* References\n#+CITE_EXPORT: csl style.csl\n#+PRINT_BIBLIOGRAPHY:\n"))

(provide 'wg21-test-support)
;;; wg21-test-support.el ends here
