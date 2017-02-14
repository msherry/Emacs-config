;; Marc Sherry

;; Custom stuff for flymake syntax checking

(require 'flymake)
(require 'flymake-line-parsers)
(require 'custom-utils)

; Disable popup windows
(setq flymake-gui-warnings-enabled nil)

; Script that runs pyflakes, pep8, and maybe pydo
(defvar python-multiple-checker-command "~/.emacs.d/plugins/pycheckers.py")

; JS checker
; On OS X, this doesn't use the IMO_HOME set in the user's init scripts, since
; emacs is an app, but rather the one set in /etc/launchd.conf
(defvar js-check-command (concat (getenv "IMO_HOME") "/scripts/jslint/imojslint"))

(defvar use-hacked-flymake-parse-line nil)
(make-variable-buffer-local 'use-hacked-flymake-parse-line)

(eval-after-load "flymake"
  '(progn
    (defun flymake-python-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list python-multiple-checker-command
              ;; -c option allows for comma-separated list of checkers
              (list local-file))))
    (add-to-list 'flymake-allowed-file-name-masks
     '("\\.py\\'" flymake-python-init))

    (defun flymake-js-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list js-check-command
              (list
               ;; "-errors"
               local-file))))
    (add-to-list 'flymake-allowed-file-name-masks
     '("\\.js\\'" flymake-js-init))
    (add-to-list 'flymake-err-line-patterns imojslint-err-line-pattern)

    (defun flymake-ruby-init ()
      (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
             (local-file  (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
        (list "ruby" (list "-c" local-file))))

    (add-to-list 'flymake-allowed-file-name-masks
     '("\\.rb\\'" flymake-ruby-init))
    (add-to-list 'flymake-allowed-file-name-masks
     '("Rakefile\\'" flymake-ruby-init))

    (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

    (add-hook 'ruby-mode-hook
     '(lambda ()
       ;; Don't want flymake mode for ruby regions in rhtml files and also on
       ;; read only files
       (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
           (flymake-mode))))

    ;; We have two separate line-parsing functions (one for js, one for
    ;; everything else), and emacs doesn't provide a way to do buffer-local
    ;; symbol-function definitions -- only symbol value. Hack up a dispatch
    ;; function
    (fset 'orig-flymake-parse-line (symbol-function 'flymake-parse-line))

    (defun flymake-parse-line (line)
      "Replacement for the original that dispatches to the stock or hacked
version, depending on the value of the variable `use-hacked-flymake-parse-line'"
      (if use-hacked-flymake-parse-line
          (imo-flymake-parse-line line)
          (orig-flymake-parse-line line)))))

(defvar flymake-modes '(python-mode c-mode-common js-mode js2-mode javascript-mode ruby-mode))   ;c-mode-common is a pain to get working

(defvar flymake-js-modes '(js-mode js2-mode javascript-mode))

(defun turn-on-flymake-if-local ()
  "Turn on flymake mode if buffer is local and writable (not over tramp)"
  (when (file-is-local-and-writable-p)
    (flymake-mode 1)))

;; Turn on flymake for local, writable files
(mapc #'(lambda (x)
          (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
            (add-hook mode-hook
                      'turn-on-flymake-if-local)))
      flymake-modes)

;; Use a different line-parsing function for javascript
(mapc #'(lambda (x)
          (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
            (add-hook mode-hook
                      'use-imo-flymake-fun)))
      flymake-js-modes)

;; Prepend a different java handler, since flymake defaults to using Makefiles
;; TODO: this doesn't actually work - running flymake on java hangs emacs
;; (push '("\\.java\\'" flymake-simple-ant-java-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)


(defun dired-mark-files-with-errors (&optional marker-char)
  "Mark all files in `dired' buffer containing Python errors.

Run `python-multiple-checker-command' on all python files in
directory, and mark all files containing errors for use in later
commands.  A prefix argument means to unmark them instead.  `.'
and `..' are never marked."
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
                ;; TODO: make this work for all types of files -- I want to
                ;; check .js this way too
                (= (call-process-shell-command
                    (mapconcat 'identity
                               `(,python-multiple-checker-command
                                 ,fn " | grep ^ERROR") " "))
                   0)))))
     "errorful file")))

;; Provide dired with a way of calling dired-mark-files-with-errors
(eval-after-load "dired"
  '(progn
    (define-key dired-mode-map (kbd "% p") 'dired-mark-files-with-errors)))


(provide 'flymake-stuff)
