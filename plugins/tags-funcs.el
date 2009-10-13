;; Marc Sherry

(defun regen-tags ()
  "Regenerate the tags file for the current working directory"
  (interactive)
  (let ((tag-file (concat default-directory "TAGS")))
    (shell-command "etags -R 2>/dev/null")
    (visit-tags-table tag-file)))


;; M-. to create a TAGS file if none exists
(defadvice find-tag (before c-tag-file activate)
   "Automatically create tags file."
   (let ((tag-file (concat default-directory "TAGS")))
     (unless (file-exists-p tag-file)
       (shell-command "etags -R 2>/dev/null"))
     (visit-tags-table tag-file)))


(global-set-key "\M-\"" 'regen-tags)

(provide 'tags-funcs)
