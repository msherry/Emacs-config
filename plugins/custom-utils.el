;; Some utility functions that can be used in multiple places

(require 'find-dired)                   ; For `find-name-arg' and `find-dired'

(defun file-is-local-and-writable-p ()
  "Determine if a buffer is local and writable. Useful for
turning off flymake and auto-save when editing remote files via
tramp."
  (and buffer-file-name
       (file-is-local-p)
       (file-writable-p (file-name-directory buffer-file-name))))


(defun file-is-local-p ()
  "Determine if a file is local, versus being edited over tramp."
  (and buffer-file-name
       (or (not (fboundp 'tramp-handle-file-remote-p))
           (not (tramp-handle-file-remote-p buffer-file-name)))))


(defun find-matching-pattern-under-dir (pattern)
  "Similar to `find-name-dired`, but doesn't prompt for directory and does partial matching.

Finds filenames containing the given pattern under the current directory. Equivalent to calling

    find . -iname \\*'PATTERN'\\*
"
  (interactive
   "sFind-name (filename wildcard): ")
  (find-dired "." (concat find-name-arg " " "*" (shell-quote-argument pattern) "*")))

(provide 'custom-utils)
