;; Some utility functions that can be used in multiple places

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

(provide 'custom-utils)
