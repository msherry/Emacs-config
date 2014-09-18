;; Marc Sherry

;; Mostly from
;; https://stackoverflow.com/questions/21246218/how-can-i-make-emacs-jedi-use-project-specific-virtualenvs

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtualenvs")

(defun project-directory (buffer-name)
  "Returns the root directory of the project that contains the
given buffer. Any directory with a .git, .svn, .hg, or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".svn")))
                (not (file-exists-p (concat root-dir ".hg")))
                (not (file-exists-p (concat root-dir ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
    root-dir))

(defun project-name (buffer-name)
  "Returns the name of the project that contains the given buffer."
  (let ((root-dir (project-directory buffer-name)))
    (if root-dir
        (file-name-nondirectory
         (directory-file-name root-dir))
      nil)))

(defun jedi-setup-venv ()
  "Activates the virtualenv of the current buffer, if one exists."
  (let ((project-name (project-name buffer-file-name)))
    (when project-name (ignore-errors (venv-workon project-name)))))

(provide 'jedi-local)
