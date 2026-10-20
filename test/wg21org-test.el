;;; wg21org-test.el --- Tests for the wg21org package entry point  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.  An editing Emacs loads wg21org.el without
;; either of wg21org's own init files, so these check what it does
;; there: what it loads, and what it leaves alone.

;;; Code:

(require 'ert)
(require 'wg21-test-support
         (expand-file-name "wg21-test-support"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'wg21org
         (expand-file-name "../wg21org"
                           (file-name-directory (or (macroexp-file-name) buffer-file-name))))

(defun wg21org-test-fresh-emacs (form)
  "Evaluate FORM in an `emacs -Q' with only wg21org on its load path.
Return what FORM printed."
  (with-temp-buffer
    (let ((status (call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil "-Q" "--batch"
                   "-L" wg21org-directory
                   "-L" (expand-file-name "packages/org-transclusion" wg21org-directory)
                   "--eval" (prin1-to-string form))))
      (should (eq 0 status))
      (string-trim (buffer-string)))))

(ert-deftest wg21org-loads-without-its-init-files ()
  (should (equal (wg21org-test-fresh-emacs
                  '(progn
                     (require 'wg21org)
                     (require 'ox)
                     (princ (list (and (org-export-get-backend 'wg21-html) t)
                                  (and (org-export-get-backend 'wg21-latex) t)
                                  (featurep 'wg21-cmptbl)
                                  (assq :eval org-babel-default-inline-header-args)
                                  org-export-allow-bind-keywords))))
                 "(t t t nil nil)")))

(ert-deftest wg21org-inline-source-defaults-stay-in-the-export ()
  (let ((before (default-value 'org-babel-default-inline-header-args)))
    (org-export-string-as "src_cpp{f();}" 'wg21-html t)
    (should (equal (default-value 'org-babel-default-inline-header-args) before))
    (org-export-string-as "src_cpp{f();}" 'html t)
    (should-not (assq :eval org-babel-default-inline-header-args))))

(ert-deftest wg21org-bind-applies-to-wg21-exports-only ()
  (let ((org-export-allow-bind-keywords nil)
        (paper "#+bind: org-export-with-sub-superscripts nil\nPattern_Syntax\n"))
    (should (string-match-p "Pattern_Syntax" (org-export-string-as paper 'wg21-html t)))
    (should-not org-export-allow-bind-keywords)
    (should (string-match-p "<sub>" (org-export-string-as paper 'html t)))
    (let ((wg21org-allow-bind-keywords nil))
      (should (string-match-p "<sub>" (org-export-string-as paper 'wg21-html t))))))

;;; wg21org-test.el ends here
