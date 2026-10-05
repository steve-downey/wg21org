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

;;; org-transclusion-test.el ends here
