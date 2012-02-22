;; Marc Sherry

;; Custom stuff for flymake syntax checking

(require 'custom-utils)

; Disable popup windows
(setq flymake-gui-warnings-enabled nil)

; On-the-fly pyflakes checking
(defvar pyflakes-command (if (eq system-type 'darwin)
                                 "pyflakes" ; dumb
                                 "pyflakes"))

; Script that runs pyflakes, pep8, and maybe pydo
(defvar python-multiple-checker-command "~/.emacs.d/plugins/pycheckers.py")

; Which checker should we use?
(defvar python-check-command pyflakes-command)

; JS checker
; On OS X, this doesn't use the IMO_HOME set in the user's init scripts, since
; emacs is an app, but rather the one set in /etc/launchd.conf
(defvar js-check-command (concat (getenv "IMO_HOME") "/scripts/jslint/imojslint"))

(defvar use-hacked-flymake-parse-line nil)
(make-variable-buffer-local 'use-hacked-flymake-parse-line)

(eval-after-load "flymake"
  '(progn
    (defun flymake-pyflakes-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list python-multiple-checker-command
              ;; -c option allows for comma-separated list of checkers
              (list "-c" "pyflakes,pep8" local-file))))
    (add-to-list 'flymake-allowed-file-name-masks
     '("\\.py\\'" flymake-pyflakes-init))

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

    ;; We have two separate line-parsing functions, and emacs doesn't provide a way
    ;; to do buffer-local symbol-function definitions -- only symbol value. Hack up
    ;; a dispatch function
    (fset 'orig-flymake-parse-line (symbol-function 'flymake-parse-line))

    (defun flymake-parse-line (line)
      "Replacement for the original that dispatches to the stock or hacked
version, depending on the value of the variable `use-hacked-flymake-parse-line'"
      (if use-hacked-flymake-parse-line
          (imo-flymake-parse-line line)
          (orig-flymake-parse-line line)))))

(defvar flymake-modes '(python-mode c-mode-common js-mode js2-mode javascript-mode))   ;c-mode-common is a pain to get working

(defvar flymake-js-modes '(js-mode js2-mode javascript-mode))

(defvar flymake-tramp-modes '())

;; parse Iskren's imojslint output into something flymake can use
(defvar imojslint-err-line-pattern; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
  '("\\(^[0-9]+\\)|##|\\([0-9]+\\)|##|\\([^|]+\\)|##|\\([^|]+\\)|##|\\([^|]+\\)"
    3 1 2 5 4))

(defun turn-on-flymake-if-local ()
  "Turn on flymake mode if buffer is local and writable"
  (when (file-is-local-and-writable-p)
    (flymake-mode 1)))

(defun use-imo-flymake-fun ()
  "Use the 5-argument flymake-parse-line, which is required for
Iskren's jslint script"
  (setq use-hacked-flymake-parse-line 1))

;; Turn on flymake for local, writable files
(mapc '(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook
                    'turn-on-flymake-if-local)))
      flymake-modes)

;; Use a different line-parsing function for javascript
(mapc '(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook
                    'use-imo-flymake-fun)))
      flymake-js-modes)

;; Prepend a different java handler, since flymake defaults to using Makefiles
;; TODO: this doesn't actually work - running flymake on java hangs emacs
;; (push '("\\.java\\'" flymake-simple-ant-java-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)


;; This doesn't strictly depend on flymake, but the side effect of having
;; python-check-command set is nice. XXX Actually, we always use
;; pyflakes-command now, since python-check-command may be set up to give us
;; PEP8 style warnings.
(defun dired-mark-files-with-errors (&optional marker-char)
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
                ;; (> (call-process-shell-command
                ;;     (concat pyflakes-command " " fn) nil nil)
                ;;    0)))))

                ;; TODO: make this work for all types of files -- I want to
                ;; check .js this way too
                (= (call-process-shell-command
                    (concat python-multiple-checker-command " -c pyflakes "
                            fn " | grep ^ERROR"))
                   0)))))
     "errorful file")))

;; Provide dired with a way of calling dired-mark-files-with-errors
(eval-after-load "dired"
  '(progn
    (define-key dired-mode-map (kbd "% p") 'dired-mark-files-with-errors)))


;; Modified version of this function - allows a 5th element in patterns to
;; specify whether a line is a warning. Used only for Iskren's imojslist script
(defun imo-flymake-parse-line (line)
  "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise."
  (let ((raw-file-name nil)
	(line-no 0)
	(err-type "e")
	(err-text nil)
	(patterns flymake-err-line-patterns)
	(matched nil))
    (while (and patterns (not matched))
      (when (string-match (car (car patterns)) line)
	(let* ((file-idx (nth 1 (car patterns)))
	       (line-idx (nth 2 (car patterns))))

	  (setq raw-file-name (if file-idx (match-string file-idx line) nil))
	  (setq line-no       (if line-idx (string-to-number (match-string line-idx line)) 0))
	  (setq err-text      (if (> (length (car patterns)) 4)
				  (match-string (nth 4 (car patterns)) line)
				(flymake-patch-err-text (substring line (match-end 0)))))
	  (or err-text (setq err-text "<no error text>"))
          ;; This first part is the part we changed
	  (if (or (and (> (length (car patterns)) 4)
                       (string-match "^[wW].*"
                                     (match-string (nth 5 (car patterns)) line)))
                  (and err-text (string-match "^[wW]arning" err-text)))
                     (setq err-type "w")
              )
	  (flymake-log 3 "parse line: file-idx=%s line-idx=%s file=%s line=%s text=%s" file-idx line-idx
		       raw-file-name line-no err-text)
	  (setq matched t)))
      (setq patterns (cdr patterns)))
    (if matched
	(flymake-ler-make-ler raw-file-name line-no err-type err-text)
      ())))


(provide 'flymake-stuff)
