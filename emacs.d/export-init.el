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

(require 'cl-lib)
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

(defconst wg21org-export-packages '(htmlize citeproc rainbow-delimiters engrave-faces)
  "Packages the HTML and LaTeX exports need.")

(let ((missing (seq-remove #'package-installed-p wg21org-export-packages)))
  (when missing
    (package-refresh-contents)
    (mapc #'package-install missing)))

(require 'org)
(require 'ox-html)
(require 'htmlize)
(require 'engrave-faces)

;; Keep org-transclusion at a known upstream revision.  The src-lines
;; extension supplies the :src, :lines, and :end syntax emitted by surround.
(defconst wg21org-root-directory
  (file-name-directory (directory-file-name user-emacs-directory))
  "Root of the wg21org exporter checkout.")
(defconst wg21org-org-transclusion-directory
  (expand-file-name "packages/org-transclusion" wg21org-root-directory)
  "Pinned org-transclusion submodule used by batch export.")
(unless (file-exists-p
         (expand-file-name "org-transclusion.el"
                           wg21org-org-transclusion-directory))
  (error "org-transclusion is missing; run: git submodule update --init packages/org-transclusion"))
(add-to-list 'load-path wg21org-org-transclusion-directory)
(setq org-transclusion-extensions '(org-transclusion-src-lines))
(require 'org-transclusion)

(defun wg21org-enable-transclusion ()
  "Materialize every transclusion in the current Org buffer, or fail.

`org-transclusion-add-all' deliberately continues after a bad link for
interactive use.  Export must be stricter: a stale UUID must not silently
remove an example from a paper."
  (when (derived-mode-p 'org-mode)
    (let ((case-fold-search t)
          (expected
           (length
            (delq
             nil
             (org-element-map (org-element-parse-buffer) 'keyword
               (lambda (keyword)
                 (when (string-equal-ignore-case
                        (org-element-property :key keyword) "transclude")
                   (save-excursion
                     (goto-char (org-element-property :begin keyword))
                     (unless (plist-get
                              (org-transclusion-keyword-string-to-plist)
                              :disable-auto)
                       t)))))))))
      ;; org-transclusion normally calls `org-indent-region' after insertion.
      ;; For source transclusions that also invokes the language indenter and
      ;; changes the spelling shown in the paper.  These directives are at
      ;; column zero, so insertion needs no Org indentation adjustment.
      (cl-letf (((symbol-function 'org-indent-region)
                 (lambda (&rest _) nil)))
        (org-transclusion-mode +1))
      (let ((materialized
             (seq-count
              (lambda (overlay)
                (eq (overlay-get overlay 'face) 'org-transclusion))
              (overlays-in (point-min) (point-max)))))
        (unless (= expected materialized)
          (error "Materialized %d of %d transclusions in %s; check file paths and UUID markers"
                 materialized expected (or buffer-file-name (buffer-name))))))))

;; Source blocks are fontified by their major mode, so code faces
;; follow the editor: rainbow-delimiters colours brackets by depth.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; LaTeX code is coloured by engrave-faces from an Emacs theme, which
;; it reads through the faces of the running Emacs.  Batch Emacs runs
;; on a terminal frame with no colours, so no theme face spec matches
;; it and every face is left unspecified.  Make the frame claim full
;; colour, and fill in what a terminal frame still leaves out:
;; - The default face stays unspecified-fg/-bg whatever the theme says;
;;   take its colours from the theme's own face spec.
;; - `color-values' knows no colour names; the standard table has them.
;; - engrave-faces restores the previous theme after reading another,
;;   which fails when there was none; start with one enabled.
;; The Modus options match the editor that wrote modus-*.css: bold
;; keywords and types, italic comments.
(when noninteractive
  (advice-add 'display-color-cells :override (lambda (&rest _) 16777216))
  (advice-add 'tty-display-color-p :override (lambda (&rest _) t))
  (advice-add 'face-attribute :around
              (lambda (face-attribute face attribute &optional frame inherit)
                (let ((value (funcall face-attribute face attribute frame inherit)))
                  (or (and (eq face 'default)
                           (member value '("unspecified-fg" "unspecified-bg"))
                           (seq-some (lambda (theme)
                                       (plist-get (face-spec-choose
                                                   (cadr (assq theme (get 'default 'theme-face))))
                                                  attribute))
                                     custom-enabled-themes))
                      value))))
  (advice-add 'color-values :around
              (lambda (color-values color &optional frame)
                (or (funcall color-values color frame)
                    (and (stringp color)
                         (tty-color-standard-values (downcase color))))))
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t)
  (load-theme 'modus-operandi-tinted t))

(setq org-adapt-indentation nil)
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
