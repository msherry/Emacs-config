;; Much of this is organized similar to:
;; http://bitbucket.org/brodie/dotfiles/src/tip/.emacs

;; Do these early so if there's an error in config we don't pollute ~/ with junk
;; files
(setq save-place-file "~/.emacs.d/emacs-places"
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Set up GUI as soon as possible
(when window-system
  (if (eq system-type 'darwin)
      (progn
        (setq default-frame-alist
              (append '((left . 15)
                        (top . 22)
                        (width . 175)
                        (height . 47))
                      default-frame-alist))))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1)))
;;   (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; This is a hacky way to do this, but we can't evaluate variables in the face
;; spec (it has a stupid stringp test), so we have to have two separate
;; custom-set-faces calls
(if window-system
    (custom-set-faces
     ;; Only one instance of custom-set-faces allowed
     '(region ((((class color) (min-colors 88) (background light))
                (:background "cyan")))))
    (custom-set-faces
     ;; Only one instance of custom-set-faces allowed
     '(region ((((class color) (min-colors 88) (background light))
                (:background "brightcyan"))))))

;; I edit this file a lot, so put it in a register
(set-register ?z '(file . "~/.emacs.d/init.el"))

(defun set-path-from-shell ()
  "Set PATH/exec-path based on the shell's configuration"
  (if (get-buffer "*set-path-from-shell*")
      (kill-buffer "*set-path-from-shell*"))
  (call-process-shell-command "echo $PATH" nil "*set-path-from-shell*")
  (with-current-buffer "*set-path-from-shell*"
    (let ((output (buffer-substring (point-min) (- (point-max) 1)))
          (emacs-path (nth 0 (last exec-path))))
      (setenv "PATH" (concat output emacs-path))
      (setq exec-path `(,@(split-string output ":") ,emacs-path))
      (kill-buffer nil))))
(set-path-from-shell)

; That doesn't work on the mac - retarded. Bring in the macports paths
; ourselves, both for emacs and subprocesses
(add-to-list 'exec-path "/opt/local/bin")
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/opt/local/mysql/bin:/opt/local/sbin:" (getenv "PATH")))

;; Set up environment
(set-language-environment "UTF-8")


;; Plugins
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor/")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor/emacs-goodies")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor/js2")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor/python-mode")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor/php-mode")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor/slime")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor/erlang")
; Others'
(require 'saveplace)
(require 'tex-site)                     ; auctex mode
(require 'erlang-start)
(require 'slime-autoloads)
(require 'flymake-point) ; shows errors in the minibuffer when highlighted
(require 'uniquify)      ; stop naming buffers <2>
(require 'comint)        ; better key handling in shell mode
(require 'highlight-beyond-fill-column)
; Mine
(require 'tags-funcs)
(require 'lisp-customization)
(require 'custom-faces)
(require 'flymake-stuff)
(require 'lambda)
; Autoloads
(autoload 'js2-mode "js2" nil t)
(autoload 'actionscript-mode "actionscript" nil t)
(autoload 'php-mode "php-mode" nil t) ; either Mac or 22 only
;; Use python-mode, instead of the crappy built-in python.el on the mac
(autoload 'python-mode "python-mode" "Python Mode." t)


;; File/mode associations
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; Indentation settings
(setq-default indent-tabs-mode nil)     ; indent with spaces instead of tabs
; Use SLIME-style indentation, instead of Emacs' huge tab stops
(setq lisp-indent-function 'common-lisp-indent-function)
(setq standard-indent 4)
(setq c-default-style "ellemtel")
(setq c-basic-offset 2)                 ; imo uses two


;; Other settings
(setq mac-command-modifier 'meta        ; 23 broke these
      ring-bell-function 'ignore)

(show-paren-mode t)
; Display
(setq transient-mark-mode t            ; on by default in 23.x
      column-number-mode t
      global-font-lock-mode t
      inhibit-splash-screen t
      truncate-partial-width-windows nil
      ediff-split-window-function 'split-window-horizontally
; Functionality
      save-place t
      require-final-newline 'visit-save ; add on both visit and save
      inferior-erlang-prompt-timeout t
      vc-delete-logbuf-window nil       ; don't close vc window when done
; put the dabbrev (regular M-/ functionality) first
      hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(setq-default show-trailing-whitespace t
              fill-column 80)           ; default of 72 is too narrow
(put 'upcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)            ; stop forcing me to spell out "yes"


; Uniquifying
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; Bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c e") 'ediff-buffers)
(global-set-key [insertchar] nil)       ; Right next to delete!!
; scroll without moving point
(global-set-key (kbd "M-n") (lambda() (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda() (interactive) (scroll-down 1)))
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "M-/") 'hippie-expand) ; better than dabbrev
; Readline in shell mode
(define-key comint-mode-map [up] 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-input)

; Better buffer switching
(iswitchb-mode t)
; Don't prompt when creating scratch buffers
(setq iswitchb-prompt-newbuffer nil)

; Give us the ability to leave certain words highlighted - always got jealous
; when i saw this in vim
(global-hi-lock-mode 1)

; Create two windows initially if we have the room. Check both current width and
; width from default-frame-alist (if present), since the frame may not have been
; resized yet
(if (> (max (window-width)
            (or (cdr (assoc 'width default-frame-alist))
                0))
       160)
    (split-window-horizontally))


;; Mode hooks
(defvar programming-modes '(python-mode js2-mode java-mode c-mode
                            lisp-mode emacs-lisp-mode sh-mode)
  "Modes used for programming")

(defun really-set-keys ()
  "Force our keys even in modes that try to override them"
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c .") 'flymake-goto-next-error))

(defun programming-mode-hook ()
  "Hook common to all programming modes"
  (add-todo-to-current-mode)
  (flyspell-prog-mode)
  (really-set-keys)
  (highlight-beyond-fill-column))

; Add a common hook to every programming mode
(mapcar '(lambda (x)
          ; Get the mode's name and turn that into a mode hook
          (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
            (add-hook mode-hook 'programming-mode-hook)))
        programming-modes)

(add-hook 'text-mode-hook
          '(lambda ()
            (flyspell-mode t)))

; Trailing whitespace is annoying in some modes
(defvar no-trailing-whitespace-modes '(shell-mode slime-repl-mode))

(mapcar '(lambda (x)
          (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
            (add-hook mode-hook '(lambda ()
                                  (setq show-trailing-whitespace nil)))))
        no-trailing-whitespace-modes)

;; (add-hook 'shell-mode-hook
;;           '(lambda ()
;;             (setq show-trailing-whitespace nil)))

;; Mouse wheel scrolling in xterm
(unless window-system
  (xterm-mouse-mode 1)
  (mouse-wheel-mode 1)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 5)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 5))))


;; ElDoc mode for modes that support it
(defvar eldoc-supported-modes '(emacs-lisp-mode))

(mapcar '(lambda (x)
          (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
            (add-hook mode-hook 'turn-on-eldoc-mode)))
        eldoc-supported-modes)

;; Modify functions that aren't quite right
(defadvice dired-mark-files-containing-regexp (before unmark-all-first
                                                      (regexp &optional marker-char))
  "Unmark marked files in dired mode before searching for new ones"
  (dired-unmark-all-files ?\r))
(ad-activate 'dired-mark-files-containing-regexp)

(defadvice py-shift-region-left (after keep-region-active
                                       (start end &optional count))
  "Keep the region active so we can do multiple shifts"
  (setq deactivate-mark nil))
(ad-activate 'py-shift-region-left)

(defadvice py-shift-region-right (after keep-region-active
                                       (start end &optional count))
  "Keep the region active so we can do multiple shifts"
  (setq deactivate-mark nil))
(ad-activate 'py-shift-region-right)
