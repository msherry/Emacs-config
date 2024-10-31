;; Marc Sherry

;; Depends on exuberant/universal ctags being installed - NOT emacs ctags
(defvar tags-cmd "ctags --langmap=c++:.ino -e -R 2>/dev/null")

(defun regen-tags ()
  "Regenerate the tags file for the current working directory"
  (interactive)
  (let ((tag-file (concat default-directory "TAGS")))
    (shell-command tags-cmd)
    (visit-tags-table tag-file)))


;; ;; M-. to create a TAGS file if none exists
;; (defadvice find-tag (before c-tag-file activate)
;;    "Automatically create tags file."
;;    (let ((tag-file (concat default-directory "TAGS")))
;;      (unless (file-exists-p tag-file)
;;        (shell-command tags-cmd))
;;      (visit-tags-table tag-file)))


(global-set-key (kbd "M-\"") 'regen-tags)
(global-set-key (kbd "M-'") 'find-tag-other-window)

(provide 'tags-funcs)
