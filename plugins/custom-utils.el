;; Some utility functions that can be used in multiple places

(defun file-is-local-and-writable-p ()
  "Determine if a buffer is local and writable. Useful for
turning off flymake and auto-save when editing remote files via
tramp."
  (and buffer-file-name
       (file-writable-p (file-name-directory buffer-file-name))
       (or (not (fboundp 'tramp-list-remote-buffers))
           (not (member
                 (current-buffer)
                 (tramp-list-remote-buffers))))))


(provide 'custom-utils)
