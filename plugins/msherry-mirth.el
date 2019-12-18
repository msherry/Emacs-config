;;; msherry-mirth.el --- Browse code at point        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Marc Sherry

;; Author: Marc Sherry <marcsherry@Marc.S-MBPro>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: Browse current code on github/etc.

;;

;;; Code:
(defvar mirth-base-url "https://github.com/msherry/%s/blob/%s/%s#L%s"
  "The base URL to use for linking to code snippets using `mirth'.

Strings to be inserted are:
1. repo name
2. branch name
3. file name
4. line number")
;;; Any string value should be safe enough -- don't prompt for confirmation.
(put 'mirth-base-url 'safe-local-variable 'stringp)

(defun mirth-find-url (pinned)
  "Find and return the URL for the current file/line.

Uses `mirth-base-url' as the URL to interpolate into, which
should be set via a dir-local variable.  If PINNED is non-nil,
return a URL pinned to the current revision, rather than the
default branch (usually master)."

  ;; TODO: replace with https://emacs.stackexchange.com/a/7378/7169
  (let* ((filepath (buffer-file-name))
         (repo-root (vc-find-root filepath ".git"))
         (repo-name (substring          ; remove trailing slash
                     (file-relative-name repo-root
                                         (file-name-directory (directory-file-name repo-root)))
                     0 -1))
         (repo-name-remote (cond ((string= repo-name "client") "desktop-client")
                                 ((string= repo-name ".emacs.d") "Emacs-config")
                                 (t repo-name)))
         (filename (file-relative-name filepath repo-root))
         (branch (if (not pinned) "master" (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "git rev-parse --short HEAD"))))
         (url (format mirth-base-url
                      repo-name-remote
                      branch
                      filename
                      (number-to-string (line-number-at-pos)))))
    url))

(defun mirth (&optional arg)
  "Browse a code repository for the current file/line.

Uses `mirth-base-url' as the URL to interpolate into, which
should be set via a dir-local variable."
  (interactive "P")
  (browse-url (mirth-find-url arg)))



(provide 'msherry-mirth)
;;; msherry-mirth.el ends here
