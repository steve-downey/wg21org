;;; export-init.el --- Batch setup for exporting papers -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:

;; Loaded by the Makefile instead of init.el.  It sets up only what
;; export needs, so a new MELPA snapshot of an editing package (magit,
;; forge, ...) cannot break the build.
;;
;; Packages come from the stable archives first: GNU and NonGNU ELPA,
;; then MELPA Stable, and plain MELPA only as a last resort.
;;
;; WG21_BABEL in the environment controls code block evaluation:
;;   yes (default)  evaluate without asking; these are our own papers
;;   no             do not evaluate; exported results are left as is

;;; Code:

(require 'package)

(setq package-user-dir
      (expand-file-name (concat "elpa-" emacs-version) user-emacs-directory))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities '(("gnu" . 30)
                                   ("nongnu" . 30)
                                   ("melpa-stable" . 20)
                                   ("melpa" . 10)))
(package-initialize)

(defconst wg21org-export-packages '(htmlize citeproc rainbow-delimiters)
  "Packages the HTML export needs.")

(let ((missing (seq-remove #'package-installed-p wg21org-export-packages)))
  (when missing
    (package-refresh-contents)
    (mapc #'package-install missing)))

(require 'org)
(require 'ox-html)
(require 'htmlize)

;; Source blocks are fontified by their major mode, so code faces
;; follow the editor: rainbow-delimiters colours brackets by depth.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)

;; ob-R needs ESS for sessions, but a plain R block runs without it.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (shell . t)
   (python . t)
   (R . t)
   (dot . t)))

(if (equal (getenv "WG21_BABEL") "no")
    (setq org-export-use-babel nil)
  (setq org-confirm-babel-evaluate nil))

;; A code block that fails to evaluate loses its stored results, but
;; Org only logs a message.  Record the failure so the build can fail.
(defvar wg21org-babel-failures 0
  "Number of code block evaluations that failed during this export.")

(advice-add 'org-babel-eval-error-notify :after
            (lambda (&rest _) (setq wg21org-babel-failures
                                    (1+ wg21org-babel-failures))))

(defun wg21org-exit ()
  "Exit Emacs, with a failure status if any code block failed to evaluate."
  (when (> wg21org-babel-failures 0)
    (message "%d code block(s) failed to evaluate; rerun with BABEL=no to keep stored results"
             wg21org-babel-failures))
  (kill-emacs (if (> wg21org-babel-failures 0) 1 0)))

;;; export-init.el ends here
