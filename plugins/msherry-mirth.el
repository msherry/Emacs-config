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

(require 'cl-lib)
(require 's)


(defvar mirth-base-url "https://github.com/${organization}/${repo}/blob/${branch}/${file}#L${lineno}"
  "The base URL to use for linking to code snippets using `mirth'.")
;;; Any string value should be safe enough -- don't prompt for confirmation.
(put 'mirth-base-url 'safe-local-variable 'stringp)


(defun mirth--shell-command (command)
  "Chomp the final newline from shell output from COMMAND."
  ;; TODO: this must exist somewhere already
  (replace-regexp-in-string "\n\\'" "" (shell-command-to-string command)))


(defun mirth--get-remote ()
  "Return the remote organization and repo names."
  ;; TODO: only works with git and github for now
  (let* ((url (mirth--shell-command "git config --get remote.origin.url"))
         organization repo)
    (unless (string-match "git@github.com:\\([^/]+\\)/\\([^.]+\\).git" url)
      (error "Not backed by a github repo"))
    (setq organization (match-string 1 url)
          repo (match-string 2 url))
    (values organization repo)))


(defun mirth-find-url (pinned)
  "Find and return the URL for the current file/line.

Uses `mirth-base-url' as the URL to interpolate into, which
should be set via a dir-local variable.  If PINNED is non-nil,
return a URL pinned to the current revision, rather than the
default branch (usually master)."

  ;; TODO: replace with https://emacs.stackexchange.com/a/7378/7169
  (let* ((filepath (buffer-file-name))
         (repo-root (vc-root-dir))
         (file (file-relative-name filepath repo-root))
         (branch (if (not pinned) "master" (mirth--shell-command "git rev-parse --short HEAD")))
         (lineno (number-to-string (line-number-at-pos))))
    (cl-multiple-value-bind (organization repo) (mirth--get-remote)
      ;; s-lex-format is incompatible with lexical binding, see
      ;; https://github.com/magnars/s.el/issues/57
      (s-format mirth-base-url 'aget
                `(("organization" . ,organization)
                  ("repo" . ,repo)
                  ("branch" . ,branch)
                  ("file" . ,file)
                  ("lineno" . ,lineno))))))

(defun mirth (&optional arg)
  "Browse a code repository for the current file/line.

Uses `mirth-base-url' as the URL to interpolate into, which
should be set via a dir-local variable."
  (interactive "P")
  (browse-url (mirth-find-url arg)))



(provide 'msherry-mirth)
;;; msherry-mirth.el ends here
