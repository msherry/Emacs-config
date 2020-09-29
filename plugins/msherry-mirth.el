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
(defvar mirth-base-url-multiline "https://github.com/${organization}/${repo}/blob/${branch}/${file}#L${lineno1}-L${lineno2}"
  "The base URL to use for linking to code snippets using `mirth', for multi-line snippets")
;;; Any string value should be safe enough -- don't prompt for confirmation.
(put 'mirth-base-url 'safe-local-variable 'stringp)
(put 'mirth-base-url-multiline 'safe-local-variable 'stringp)


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
      (error "ERROR: Not backed by a github repo"))
    (setq organization (match-string 1 url)
          repo         (match-string 2 url))
    (values organization repo)))


(defun mirth--get-branch (&optional pinned)
  "Return a revision for the current branch.

If PINNED is non-nil, returns a short revision SHA.  With two
prefix args (numeric value 16), return the SHA of the current
head of master, even if that is not the checked-out revision.
Otherwise, return \"master\"."
  (if (not pinned)
      "master"
    (let ((refname (if (= 16 (prefix-numeric-value pinned))
                       "master"
                     "HEAD")))
      (mirth--shell-command (format "git rev-parse --short %s" refname)))))


(defun mirth-find-url (pinned beg end)
  "Find and return the URL for the current file/line.

Uses `mirth-base-url'/`mirth-base-url-multiline' as the URL to
interpolate into.  If PINNED is non-nil, return a URL pinned to a
specific SHA (current master or current branch, depending on the
number of prefix args), rather than the default branch (usually
master).

If the region is active, BEG and END represent points in the
lines to be linked.  Otherwise, they are ignored."
  (let* ((repo-root (vc-root-dir))
         (file (file-relative-name (buffer-file-name) repo-root))
         (branch (mirth--get-branch pinned))
         (lineno (number-to-string (line-number-at-pos)))
         (lineno1 (number-to-string (line-number-at-pos beg)))
         (lineno2 (number-to-string (line-number-at-pos end))))
    (cl-multiple-value-bind (organization repo) (mirth--get-remote)
      ;; s-lex-format is incompatible with lexical binding, see
      ;; https://github.com/magnars/s.el/issues/57
      (if (region-active-p)
          ;; TODO: this duplication is ugly, but it saves us from having to
          ;; know that Github urls are formatted line #L1-L22 in this
          ;; function. Can we clean it up?
          (s-format mirth-base-url-multiline 'aget
                `((organization . ,organization)
                  (repo . ,repo)
                  (branch . ,branch)
                  (file . ,file)
                  (lineno1 . ,lineno1)
                  (lineno2 . ,lineno2)))
        (s-format mirth-base-url 'aget
                  `((organization . ,organization)
                    (repo . ,repo)
                    (branch . ,branch)
                    (file . ,file)
                    (lineno . ,lineno)))))))

(defun mirth (&optional arg beg end)
  "Browse a code repository for the current file/line.

With a prefix arg, browse at the current revision, rather than
master."
  (interactive "P\nr")
  (browse-url (mirth-find-url arg beg end)))



(provide 'msherry-mirth)
;;; msherry-mirth.el ends here
