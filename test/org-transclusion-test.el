;;; org-transclusion-test.el --- Tests for source transclusion -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest source-transclusion-preserves-text-and-omits-markers ()
  (let* ((directory (make-temp-file "wg21org-transclusion-" t))
         (source (expand-file-name "example.cpp" directory))
         (uuid "381911b5-3d9d-4dfe-aad9-bfc911636647"))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "// " uuid "\n"
                    "int f(\n"
                    "    int value) {\n"
                    "  return value;\n"
                    "}\n"
                    "// " uuid " end\n"))
          (with-temp-buffer
            (org-mode)
            (org-transclusion-mode -1)
            (insert (format "#+transclude: [[file:%s::%s]] :lines 2- :src cpp :end \"%s end\"\n"
                            source uuid uuid))
            (wg21org-enable-transclusion)
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "int f(\n    int value)" text))
              (should-not (string-match-p (regexp-quote uuid) text)))))
      (delete-directory directory t))))

(ert-deftest source-transclusion-fails-when-uuid-is-stale ()
  (let* ((directory (make-temp-file "wg21org-transclusion-" t))
         (source (expand-file-name "example.cpp" directory)))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "int answer = 42;\n"))
          (with-temp-buffer
            (org-mode)
            (org-transclusion-mode -1)
            (insert (format "#+transclude: [[file:%s::missing-uuid]] :lines 2- :src cpp :end \"missing-uuid end\"\n"
                            source))
            (should-error (wg21org-enable-transclusion))))
      (delete-directory directory t))))

(ert-deftest export-entry-points-fail-when-transclusion-is-stale ()
  (let* ((directory (make-temp-file "wg21org-transclusion-export-" t))
         (paper (expand-file-name "paper.org" directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file paper
            (insert "#+TITLE: Missing example\n"
                    "#+transclude: [[file:missing.cpp::missing-uuid]] :src cpp\n"))
          ;; Errors from mode hooks are swallowed by file visiting.  This test
          ;; exercises the actual synchronous export entry points instead.
          (setq buffer (find-file-noselect paper))
          (with-current-buffer buffer
            (dolist (export '(my-wg21-export-to-html my-wg21-export-to-latex))
              (org-transclusion-mode -1)
              (should-error (funcall export)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest disabled-and-literal-transclusions-are-not-required ()
  (with-temp-buffer
    (org-mode)
    (org-transclusion-mode -1)
    (insert "#+transclude: [[file:missing.cpp::missing]] :disable-auto\n"
            "#+begin_example\n"
            "#+transclude: [[file:missing.cpp::example]]\n"
            "#+end_example\n"
            "#+begin_src text\n"
            "#+transclude: [[file:missing.cpp::source]]\n"
            "#+end_src\n")
    (wg21org-enable-transclusion)
    (should-not
     (seq-some (lambda (overlay)
                 (eq (overlay-get overlay 'face) 'org-transclusion))
               (overlays-in (point-min) (point-max))))))

;;; org-transclusion-test.el ends here
