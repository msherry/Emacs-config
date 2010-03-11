;; Marc Sherry

;; Custom stuff for flymake syntax checking

; Disable popup windows
(setq flymake-gui-warnings-enabled nil)

; On-the-fly pyflakes checking
(defvar python-check-command (if (eq system-type 'darwin)
                                 "pyflakes-2.6" ; dumb
                                 "pyflakes"))
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list python-check-command (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(defvar flymake-modes '(python-mode c-mode-common))   ;c-mode-common is a pain to get working

(defun turn-on-flymake-if-local ()
  "Turn on flymake mode if buffer's directory is writable and
tramp is not loaded, or this file isn't being edited via tramp."
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (or (not (fboundp 'tramp-list-remote-buffers))
                 (not (member
                       (current-buffer)
                       (tramp-list-remote-buffers)))))
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
;; python-check-command set is nice.
(defun dired-mark-python-with-errors (&optional marker-char)
  "Run `python-check-command' on all python files in directory, and mark all
files containing errors for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked."
  (interactive
   (list (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (if (get-buffer "*dired-mark-python-with-errors*")
        (kill-buffer "*dired-mark-python-with-errors*"))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
          (not (eolp))			; empty line
          (let ((fn (dired-get-filename nil t)))
            (when (and fn (file-readable-p fn)
                       (not (file-directory-p fn))
                       (string-match "\.py$" fn))
              (progn
                (message "Checking %s" fn)
                (> (call-process-shell-command (concat python-check-command " " fn) nil "*dired-mark-python-with-errors*")
                   0)))))
     "errorful file")
    (if (get-buffer "*dired-mark-python-with-errors*")
        (kill-buffer "*dired-mark-python-with-errors*"))))

;; Provide dired with a way of calling dired-mark-python-with-errors
(when (load "dired" t)
  (define-key dired-mode-map (kbd "% p") 'dired-mark-python-with-errors))

(provide 'flymake-stuff)
