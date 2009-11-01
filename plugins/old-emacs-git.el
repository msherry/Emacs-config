;; git wasn't included in vc-mode until emacs 22.2 - ubuntu 8.04 ships with
;; 22.1. Doh. git-core includes the necessary files, though, so include them
;; when necessary

;; TODO: Not perfect - first load of a git-controlled file doesn't have this
;; ready yet, so we have to close and re-open it. Kind of annoying
(eval-after-load 'vc
  (when (or (< emacs-major-version 22)
            (and (= emacs-major-version 22)
                 (< emacs-minor-version 2)))
    '(progn
       (load "/usr/share/doc/git-core/contrib/emacs/git.el")
       (load "/usr/share/doc/git-core/contrib/emacs/git-blame.el")
       (load "/usr/share/doc/git-core/contrib/emacs/vc-git.el")
       (add-to-list 'vc-handled-backends 'GIT))))

(provide 'old-emacs-git)
