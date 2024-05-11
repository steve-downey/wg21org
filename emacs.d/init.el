;; Save any custom set variable in exordium-custom-file rather than at the end of init.el:
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir
      (locate-user-emacs-file (concat "elpa-" emacs-version)))

(when (fboundp 'native-comp-available-p)
  (setq package-native-compile (native-comp-available-p)))
(package-initialize)

;; Load the packages we need if they are not installed already
(let ((package-pinned-packages (append
                                '((use-package             . "melpa")
                                  (diminish                . "melpa")
                                  (bind-key                . "melpa"))))
      (has-refreshed nil))

  (defun update-package (p  has-refreshed)
    (unless (package-installed-p p)
      (unless has-refreshed
        (message "Refreshing package database...")
        (package-refresh-contents)
        (setq has-refreshed t)
        (message "Done."))
      (package-install p)))

  (dolist (pkg package-pinned-packages)
    (let ((p (car pkg)))
      (update-package p has-refreshed))))

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;;; remove a package from the builtin list so it can be upgraded
(defun exordium-ignore-builtin (pkg)
  (assq-delete-all pkg package--builtins)
  (assq-delete-all pkg package--builtin-versions))


;;; Org mode

(defcustom exordium-enable-org-export t
  "Configure org mode for code export and babel. Setting this to
  nil makes emacs starts a little bit faster, if you don't need
  to use this feature."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-no-org-babel-confirm nil
  "Disable confirmation requests when evaluating code blocks when
  using org-babel. Setting to non-nil could conceivably result in
  accidentally executing code."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-org-export-css nil
  "Export from org mode using a css style sheet, rather than
  inline styles. This allows more control over the appearance, at
  the cost of having to maintain a stylesheet."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-org-export-css-stylesheet ""
  "The stylesheet to use in html based org export. This will be
  loaded into `org-html-head' and exported. For example,

     <link rel=\"stylesheet\" type=\"text/css\" href=\"http://sundev3.dev.bloomberg.com/~sdowney/smd-zenburn.css\" />

  To generate a basic css file that matches your theme, use
  `org-html-htmlize-generate-css' which will create a buffer with
  css definitions for all currently defined faces."
  :group 'exordium
  :type  'string)


(exordium-ignore-builtin 'org)

(defun exordium--org-babel-after-execute ()
  "Redisplay inline images in subtree if cursor in source block with :result graphics.

Rationale:
For some reason `org-babel-execute' is not producing images from .dot format (`org-version' 9.5.4).
This is a spin off https://stackoverflow.com/a/66911315/519827, but REFRESH is set to nil."
  (when (org-in-src-block-p)
    (let (beg end)
      (save-excursion
        (org-mark-subtree)
        (setq beg (point))
        (setq end (mark)))
      (when-let ((info (org-babel-get-src-block-info t))
                 (params (org-babel-process-params (nth 2 info)))
                 (result-params (cdr (assq :result params)))
                 ((string-match-p "graphics" result-params)))
        (org-display-inline-images nil nil beg end)))))

(use-package org
  :commands (org-mode)
  :mode (("\\.org\\'" . org-mode))
  :bind
  (:map org-mode-map
        ([remap org-toggle-comment] . iedit-mode))
  :custom
  (org-startup-folded t)
  (org-log-into-drawer t)
  (org-startup-truncated nil)
  (org-startup-with-inline-images t)
  (org-src-fontify-natively t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate (not exordium-no-org-babel-confirm)
                              "Turn off the confirmation for code eval when using org-babel.")
  (org-support-shift-select t)
  :config
  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)


  (add-hook 'org-babel-after-execute-hook #'exordium--org-babel-after-execute)

  ;; TODO: delete `exordium-enable-org-export'??
  (when exordium-enable-org-export
    ;; Enable org-babel for perl, ruby, sh, python, emacs-lisp, C, C++, etc
    ;; TODO: add extra languages configurable by user
    (org-babel-do-load-languages
     'org-babel-load-languages
     `((perl       . t)
       (ruby       . t)
       (shell      . t)
       (python     . t)
       (emacs-lisp . t)
       (C          . t)
       (dot        . t)
       (sql        . t)))))

(exordium-ignore-builtin 'htmlize)

(use-package htmlize
  :ensure t)

(use-package ox-html
  :ensure org
  :after (org))

(use-package ox-html
  :ensure org
  :after (org)
  :if exordium-org-export-css
  :custom
  (org-html-htmlize-output-type 'css
                                "Configure export using a css style sheet")
  (org-html-head exordium-org-export-css-stylesheet
                 "Configure export using a css style sheet"))

(use-package ox-md
  :ensure org
  :after (org)
  :if exordium-enable-org-export)

(use-package ox-beamer
  :ensure org
  :after (org)
  :if exordium-enable-org-export)

(use-package ox-odt
  :ensure org
  :after (org)
  :if exordium-enable-org-export)

(use-package ox-publish
  :ensure org
  :after (org)
  :if exordium-enable-org-export)

(use-package ox-gfm
  :ensure t
  :after (org)
  :if exordium-enable-org-export)

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((dot . t))))


(setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

(setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((plantuml . t))))

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((ditaa . t))))

(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

(setq org-support-shift-select 'always)


;; Reveal.js + Org mode
(use-package org-re-reveal
  :config
  (setq org-re-reveal-root "file:////home/sdowney/bld/reveal.js"))



(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-classes
                '("memoir" "\\documentclass{memoir}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t
         ("pdflatex"))
        ("T1" "fontenc" t
         ("pdflatex"))
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "titletoc" nil)
        ("" "hyperref" nil)))


(use-package org-transclusion
  :after org)

(use-package engrave-faces
  :ensure t
  :init
  (setq org-latex-src-block-backend 'engraved)
  (setq org-latex-engraved-theme t))

(use-package with-editor)

(use-package magit
  :ensure t)
