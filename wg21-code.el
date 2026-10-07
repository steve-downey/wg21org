;; wg21-code.el --- source-code conventions for WG21 papers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steve Downey

;;; Commentary:

;; Org already has a language on each source block.  This module adds two
;; document-level conveniences without inventing Markdown attributes:
;;
;;   #+WG21_CODE_LANGUAGE: C++
;;   #+WG21_CPP_KEYWORDS: inspect reflexpr
;;
;; The first fills only otherwise-unlabelled source blocks.  An explicit
;; language always wins; `text' and `fundamental' are useful raw overrides.
;; The second extends Emacs' own C++ font-lock rules during export.

;;; Code:

(require 'cc-mode)
(require 'org-element)
(require 'ob-core)

(defvar wg21-code-language "C++"
  "Default language for otherwise-unlabelled source blocks.")

(defvar wg21-code-current-cpp-keywords nil
  "Additional C++ keywords active while one source block is exported.")

(defun wg21-code-inline-header-args ()
  "Return Org inline-source defaults suitable for a WG21 paper.

Inline source is presentation code unless the paper says otherwise: export
the code and do not evaluate it.  Buffer properties and element parameters
are merged later by Org and therefore override these defaults."
  (org-babel-merge-params
   org-babel-default-inline-header-args
   '((:exports . "code") (:eval . "never-export"))))

;; Babel expands inline source before the backend's element transcoders run,
;; so establish the presentation-safe defaults as soon as a WG21 backend is
;; loaded.  Org merges buffer properties and element parameters afterward.
(setq org-babel-default-inline-header-args
      (wg21-code-inline-header-args))

(define-derived-mode wg21-c++-mode c++-mode "WG21 C++"
  "C++ mode with the current paper's proposed keywords highlighted."
  (when wg21-code-current-cpp-keywords
    (font-lock-add-keywords
     nil `((,(regexp-opt wg21-code-current-cpp-keywords 'symbols)
            (0 'font-lock-keyword-face t))) 'append)))

;; Org spells the language in source blocks as C++, but its normal major mode
;; is c++-mode.  Route it through the small derived mode above.
(setf (alist-get "C++" org-src-lang-modes nil nil #'equal) 'wg21-c++)
(setf (alist-get "cpp" org-src-lang-modes nil nil #'equal) 'wg21-c++)

(defun wg21-code-apply-default-language (tree _backend info)
  "Apply INFO's document-level source language to unlabelled blocks in TREE."
  (when-let* ((language (plist-get info :wg21-code-language))
              ((not (string-empty-p language))))
    (org-element-map tree 'src-block
      (lambda (block)
        (unless (org-element-property :language block)
          (org-element-put-property block :language language)))
      info))
  tree)

(defun wg21-code-raw-p (src-block)
  "Non-nil when SRC-BLOCK explicitly requests unhighlighted text."
  (member (downcase (or (org-element-property :language src-block) ""))
          '("text" "fundamental" "raw")))

(defun wg21-code-expand-table-vertical-bars (element)
  "Return ELEMENT with Org's table-safe vertical bars expanded in code.
An actual | would end the table cell, while Org does not normally expand the
`\\vert{}' entity inside code or verbatim markup."
  (if (and (org-element-lineage element '(table-cell) t)
           (string-search "\\vert{}" (org-element-property :value element)))
      (let ((copy (org-element-copy element)))
        (org-element-put-property
         copy :value
         (string-replace "\\vert{}" "|" (org-element-property :value element)))
        copy)
    element))

(provide 'wg21-code)
;;; wg21-code.el ends here
