;; wg21-git.el --- git metadata for WG21 papers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steve Downey

;; Author: Steve Downey <sdowney@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Where a paper's source lives, shared by the HTML and LaTeX exporters:
;; the repository, the file, its version and commit, and permalinks to
;; the file and to the line of each headline in it.  Papers can also
;; use `wg21-git-string' and `wg21-source-url' in macros.

;;; Code:

(require 'format-spec)
(require 'ox)


(defcustom wg21-git-remote "origin"
  "Git remote whose URL is used when #+SOURCE_REPO is not given."
  :group 'my-export-wg21
  :type 'string)

(defcustom wg21-forge-blob-url-formats
  '(("\\`https?://github\\.com/" "%r/blob/%c/%p" "?plain=1#L%l")
    ("\\`https?://gitlab\\.com/" "%r/-/blob/%c/%p" "?plain=1#L%l")
    ("" "%r/src/commit/%c/%p" "?display=source#L%l"))
  "Permalink formats for a file at a commit, per forge.
Each entry is (REGEXP FILE-FORMAT LINE-FORMAT); the first REGEXP
matching the repository URL wins.  In FILE-FORMAT, %r is the
repository URL, %c the commit, and %p the path of the file within
the repository.  LINE-FORMAT is appended to link to line %l of the
file's source, rather than to a rendered view where lines have no
anchors.  The catch-all entry is the Gitea/Forgejo layout."
  :group 'my-export-wg21
  :type '(repeat (list regexp string string)))

(defun wg21-git-output (&rest args)
  "Run git with ARGS in `default-directory'; return its output.
Return nil if git fails or prints nothing.  Nil ARGS are dropped, so
a missing `buffer-file-name' does not become a literal argument."
  (with-temp-buffer
    (when (and (eql 0 (apply #'process-file "git" nil '(t nil) nil
                             (delq nil args)))
               (> (buffer-size) 0))
      (buffer-string))))

(defun wg21-git-string (&rest args)
  "Run git with ARGS in `default-directory'; return its first output line.
Return nil if git fails or prints nothing."
  (let ((output (apply #'wg21-git-output args)))
    (and output (car (split-string output "\n")))))

(defun wg21-git-https-url (url)
  "Turn the git remote URL into the https URL of its web page.
Handles scp-style (git@host:owner/repo), ssh://, and http(s) remotes.
An ssh port is dropped, since it says nothing about the web port."
  (when url
    (let ((url (string-remove-suffix ".git" (string-remove-suffix "/" url))))
      (cond
       ((string-match "\\`https?://" url) url)
       ((string-match "\\`ssh://\\(?:[^@/]+@\\)?\\([^:/]+\\)\\(?::[0-9]+\\)?/\\(.*\\)\\'" url)
        (format "https://%s/%s" (match-string 1 url) (match-string 2 url)))
       ((string-match "\\`\\(?:[^@/]+@\\)?\\([^:/]+\\):\\(.*\\)\\'" url)
        (format "https://%s/%s" (match-string 1 url) (match-string 2 url)))))))

(defun wg21-git-blob-url (repo commit path &optional line)
  "Return a permalink to PATH at COMMIT in the web view of REPO.
With LINE, link to that line of the file's source.
See `wg21-forge-blob-url-formats'."
  (when (and repo commit path)
    (let ((formats (cdr (seq-find (lambda (entry) (string-match-p (car entry) repo))
                                  wg21-forge-blob-url-formats)))
          (spec `((?r . ,repo) (?c . ,commit) (?p . ,path) (?l . ,line))))
      (concat (format-spec (nth 0 formats) spec)
              (and line (format-spec (nth 1 formats) spec))))))

(defun wg21-source-url (&optional file)
  "Return a permalink to FILE, by default the current buffer's file, at HEAD.
Intended for use in macros, e.g.
  #+MACRO: permalink (eval (wg21-source-url))"
  (let ((file (or file (buffer-file-name))))
    (when file
      (let ((default-directory (file-name-directory file)))
        (wg21-git-blob-url
         (wg21-git-https-url
          (wg21-git-string "remote" "get-url" wg21-git-remote))
         (wg21-git-string "rev-parse" "HEAD")
         (wg21-git-string "ls-files" "--full-name" "--" file))))))

(defun wg21-git-metadata (info)
  "Return a plist of git metadata for the document being exported.
The result is computed once per export and kept in INFO."
  (or (plist-get info :wg21-git)
      (let ((git (wg21-git--metadata info)))
        (wg21-git--warn-unpushed git info)
        (plist-get (plist-put info :wg21-git git) :wg21-git))))

(defun wg21-git--warn-unpushed (git info)
  "Warn when the commit that GIT's source links name is on no remote.
Such links point at a commit the forge does not have, so they lead
nowhere until it is pushed.  INFO is a plist holding export options."
  (let* ((input (plist-get info :input-file))
         (default-directory (if input (file-name-directory input)
                              default-directory))
         (commit (plist-get git :commit)))
    (when (and commit (plist-get git :url)
               (not (wg21-git-string "branch" "-r" "--contains" commit)))
      (message "wg21: commit %s is on no remote branch; source links will lead nowhere until it is pushed"
               (substring commit 0 (min 12 (length commit)))))))

(defun wg21-git--metadata (info)
  "Return a plist of git metadata for the document being exported.
Each of :repo, :file, :version and :commit comes from the matching
keyword (SOURCE_REPO, SOURCE_FILE, SOURCE_VERSION, GIT_COMMIT) if the
document sets it, and otherwise from git.  :url is a permalink to the
file at the commit.  Everything is nil outside a git work tree.
INFO is a plist holding export options."
  (let* ((input (plist-get info :input-file))
         (default-directory (if input (file-name-directory input)
                              default-directory))
         (keyword (lambda (prop)
                    (let ((value (org-trim
                                  (org-element-interpret-data
                                   (plist-get info prop)))))
                      (and (not (string-empty-p value)) value))))
         (repo (or (funcall keyword :source_repo)
                   (wg21-git-https-url
                    (wg21-git-string "remote" "get-url" wg21-git-remote))))
         (file (or (funcall keyword :source_file)
                   (and input (wg21-git-string "ls-files" "--full-name"
                                               "--" input))))
         (version (or (funcall keyword :source_version)
                      (wg21-git-string "describe" "--always" "--long" "--all"
                                       "--dirty" "--tags")))
         (commit (or (funcall keyword :git_commit)
                     (wg21-git-string "rev-parse" "HEAD"))))
    (list :repo repo :file file :version version :commit commit
          :url (and file (wg21-git-blob-url repo commit file)))))

(defun wg21-git--headline-lines (info)
  "Map each headline to its line in the committed source of the document.
Return a hash table from headline element to line number.  Headlines
are found in document order in `git show COMMIT:FILE', so the links
point at the file the permalinks name, not at unsaved edits.  A
headline with no match there, such as a new one or one from
#+INCLUDE, is absent.  INFO is a plist holding export options."
  (let* ((git (wg21-git-metadata info))
         (lines (make-hash-table :test #'eq))
         (input (plist-get info :input-file))
         (default-directory (if input (file-name-directory input)
                              default-directory))
         (text (and (plist-get git :commit) (plist-get git :file)
                    (wg21-git-output "show" (concat (plist-get git :commit) ":"
                                                    (plist-get git :file))))))
    (when text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (org-element-map (plist-get info :parse-tree) 'headline
          (lambda (headline)
            (let ((start (point))
                  (title (org-element-property :raw-value headline)))
              (if (re-search-forward (concat "^\\*+ .*" (regexp-quote title)) nil t)
                  (puthash headline (line-number-at-pos) lines)
                (goto-char start))))
          info)))
    lines))

(defun wg21-git-headline-url (headline info)
  "Return a permalink to HEADLINE's line in the document's source, or nil.
INFO is a plist holding export options."
  (let* ((lines (or (plist-get info :wg21-headline-lines)
                    (plist-get (plist-put info :wg21-headline-lines
                                          (wg21-git--headline-lines info))
                               :wg21-headline-lines)))
         (line (gethash headline lines))
         (git (wg21-git-metadata info)))
    (and line
         (wg21-git-blob-url (plist-get git :repo) (plist-get git :commit)
                            (plist-get git :file) line))))

(provide 'wg21-git)
;;; wg21-git.el ends here
