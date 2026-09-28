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

(provide 'wg21-test-support)
;;; wg21-test-support.el ends here
