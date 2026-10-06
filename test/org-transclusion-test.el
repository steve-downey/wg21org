;;; org-transclusion-test.el --- Tests for source transclusion -*- lexical-binding: t; -*-

(require 'ert)
(require 'ox-publish)

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
            (dolist (export '(my-wg21-export-to-html my-wg21-export-to-latex
                              my-wg21-export-as-html my-wg21-export-as-latex))
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

;; Most papers below transclude this one example source.
(defun wg21org-transclusion-test-paper (directory)
  "Write a paper with one source transclusion into DIRECTORY; return its name."
  (let ((source (expand-file-name "example.cpp" directory))
        (paper (expand-file-name "paper.org" directory))
        (uuid "5b0f5c8e-2f53-4a43-9a43-0d6c3b8f1e27"))
    (with-temp-file source
      (insert "// " uuid "\n"
              "int transcludedfunction();\n"
              "// " uuid " end\n"))
    (with-temp-file paper
      (insert "#+TITLE: Transclusion\n"
              (format "#+transclude: [[file:example.cpp::%s]] :lines 2- :src cpp :end \"%s end\"\n"
                      uuid uuid)))
    paper))

(ert-deftest transclusion-can-be-enabled-twice-in-one-buffer ()
  (with-temp-buffer
    (let ((directory (make-temp-file "wg21org-transclusion-" t)))
      (unwind-protect
          (progn
            (insert-file-contents (wg21org-transclusion-test-paper directory))
            (setq default-directory (file-name-as-directory directory))
            (org-mode)
            (wg21org-enable-transclusion)
            (wg21org-enable-transclusion)
            (should (string-match-p "transcludedfunction"
                                    (buffer-string))))
        (delete-directory directory t)))))

(ert-deftest html-then-latex-export-in-one-session-transcludes-both ()
  (let* ((directory (make-temp-file "wg21org-transclusion-export-" t))
         (paper (wg21org-transclusion-test-paper directory))
         (org-export-use-babel nil)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect paper))
          (with-current-buffer buffer
            (let ((html (my-wg21-export-to-html))
                  (latex (my-wg21-export-to-latex)))
              (dolist (file (list html latex))
                (with-temp-buffer
                  (insert-file-contents (expand-file-name file directory))
                  (should (string-match-p "transcludedfunction"
                                          (buffer-string))))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest export-as-buffer-entry-points-transclude ()
  (let* ((directory (make-temp-file "wg21org-transclusion-export-" t))
         (paper (wg21org-transclusion-test-paper directory))
         (org-export-use-babel nil)
         (org-export-show-temporary-export-buffer nil)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect paper))
          (with-current-buffer buffer
            (dolist (export '(my-wg21-export-as-html my-wg21-export-as-latex))
              (let ((output (funcall export nil nil nil t)))
                (should (string-match-p
                         "transcludedfunction"
                         (with-current-buffer output (buffer-string))))
                (kill-buffer output)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest publish-to-html-transcludes ()
  (let* ((directory (make-temp-file "wg21org-transclusion-publish-" t))
         (paper (wg21org-transclusion-test-paper directory))
         (pub-dir (expand-file-name "out" directory))
         (org-export-use-babel nil)
         (org-publish-timestamp-directory
          (file-name-as-directory (expand-file-name "timestamps" directory)))
         (org-publish-cache nil))
    (unwind-protect
        (progn
          (org-publish-initialize-cache "wg21org-transclusion-test")
          (let ((html (my-wg21-publish-to-html '(:body-only t) paper pub-dir)))
            (with-temp-buffer
              (insert-file-contents html)
              (should (string-match-p "transcludedfunction" (buffer-string)))))
          ;; The paper was not visited before publishing; it is not left open.
          (should-not (find-buffer-visiting paper)))
      (delete-directory directory t))))

;;; org-transclusion-test.el ends here
