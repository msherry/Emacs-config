;; Marc Sherry

;; Custom stuff for flymake syntax checking

(require 'custom-utils)

; Disable popup windows
(setq flymake-gui-warnings-enabled nil)

; On-the-fly pyflakes checking
(defvar pyflakes-command (if (eq system-type 'darwin)
                                 "pyflakes-2.6" ; dumb
                                 "pyflakes"))

; Script that runs pyflakes, pep8, and maybe pydo
(defvar python-multiple-checker-command "~/.emacs.d/plugins/pycheckers.py")

; Which checker should we use?
(defvar python-check-command pyflakes-command)


(eval-after-load "flymake"
  '(progn
    (defun flymake-pyflakes-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list python-multiple-checker-command
              (list "-c" "pyflakes,pep8" temp-file))))
        ;; (list python-check-command (list local-file))))
    (add-to-list 'flymake-allowed-file-name-masks
     '("\\.py\\'" flymake-pyflakes-init))))

(defvar flymake-modes '(python-mode c-mode-common))   ;c-mode-common is a pain to get working

(defun turn-on-flymake-if-local ()
  "Turn on flymake mode if buffer is local and writable"
  (when (file-is-local-and-writable-p)
    (flymake-mode 1)))

(mapc '(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook
                    'turn-on-flymake-if-local)))
      flymake-modes)

;; Prepend a different java handler, since flymake defaults to using Makefiles
;; TODO: this doesn't actually work - running flymake on java hangs emacs
;; (push '("\\.java\\'" flymake-simple-ant-java-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)


;; This doesn't strictly depend on flymake, but the side effect of having
;; python-check-command set is nice. XXX Actually, we always use
;; pyflakes-command now, since python-check-command may be set up to give us
;; PEP8 style warnings.
(defun dired-mark-python-with-errors (&optional marker-char)
  "Run `python-check-command' on all python files in directory, and mark all
files containing errors for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked."
  (interactive
   (list (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
          (not (eolp))			; empty line
          (let ((fn (dired-get-filename nil t)))
            (when (and fn (file-readable-p fn)
                       (not (file-directory-p fn))
                       (string-match "\.py$" fn))
              (progn
                (message "Checking %s" fn)
                (> (call-process-shell-command
                    (concat pyflakes-command " " fn) nil nil)
                   0)))))
     "errorful file")))

;; Provide dired with a way of calling dired-mark-python-with-errors
(eval-after-load "dired"
  '(progn
    (define-key dired-mode-map (kbd "% p") 'dired-mark-python-with-errors)))

(provide 'flymake-stuff)
