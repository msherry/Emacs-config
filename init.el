;; Much of this is organized similar to:
;; http://bitbucket.org/brodie/dotfiles/src/tip/.emacs

;; Do these early so if there's an error in config we don't pollute ~/ with junk
;; files
(setq save-place-file "~/.emacs.d/emacs-places"
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

; Seed RNG
(random t)

;; While emacs23 handles greek poorly on the mac, use a different font. See last
;; http ref in plugins/lambda.el. Also stop using a chinese font for japanese
;; kanji. "fontset-startup" (or fontsets at all, it seems) don't exist in 22, so
;; only do this on the mac for now
(when (eq system-type 'darwin)
  (set-fontset-font "fontset-startup"
		    'greek-iso8859-7
		    "-apple-Andale_Mono-medium-normal-normal-*-14-*-*-*-p-0-iso10646-1"))
;; TODO: this seems to override more than just japanese - even greek
;; (set-fontset-font "fontset-startup"
;;                   'japanese-jisx0208
;;                   "-apple-Osaka-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")

;; Set up GUI as soon as possible
(when window-system
  (if (eq system-type 'darwin)
      (setq default-frame-alist
            (append `((left . ,(cond ((< emacs-major-version 23) 15)
                                     (t 8)))
                      (top . 22)
                      (width . 175)
                      (height . ,(cond ((< emacs-major-version 23) 47)
                                       (t 52))))
                    default-frame-alist)))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1)))
;;   (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; I edit this file a lot, so put it in a register
(set-register ?z '(file . "~/.emacs.d/init.el"))
(set-register ?l '(file . "~/Desktop/notes-for-lyd.txt"))
(set-register ?g '(file . "~/imo.im/trunk/imo/imop/GraphHead.py"))
(set-register ?t '(file . "~/TODO.org"))
(set-register ?b '(file . "/imo25:imo.im/trunk/scripts/host/root/run_on_first_boot"))
(set-register ?k '(file . "/athens:imo.im/branches/kodama"))

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
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/opt/local/mysql/bin:/opt/local/sbin:" (getenv "PATH")))

;; Set up environment
(set-language-environment "UTF-8")

;; Plugins - add plugins dir, vendors dir, and all dirs under vendor
;; excluding . and ..
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor")
(dolist (dir (directory-files "~/.emacs.d/plugins/vendor" t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

; Others'
(require 'saveplace)
; Doesn't work on Ubuntu 8.04 with emacs 23
(when (not (and (>= emacs-major-version 23)
		(eq system-type 'gnu/linux)))
  (require 'tex-site))                     ; auctex mode
(require 'erlang-start)
(require 'slime-autoloads)
(require 'flymake-point) ; shows errors in the minibuffer when highlighted
(require 'uniquify)      ; stop naming buffers <2>
(require 'comint)        ; better key handling in shell mode
(require 'highlight-beyond-fill-column)
(require 'tramp)
(require 'vc-svn)
; Mine
(require 'tags-funcs)
(require 'lisp-customization)
(require 'custom-faces)
(require 'flymake-stuff)
(when (< emacs-major-version 23)
  (require 'old-emacs-git))
(require 'lambda)
(require 'totd)
; Autoloads
(autoload 'js2-mode "js2" nil t)
(autoload 'actionscript-mode "actionscript" nil t)
(autoload 'php-mode "php-mode" nil t) ; either Mac or 22 only
;; Use python-mode, instead of the crappy built-in python.el on the mac
(autoload 'python-mode "python-mode" "Python Mode." t)
(autoload 'turn-on-cldoc-mode "cldoc" "CL docs" t)


;; File/mode associations
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; Indentation settings
(setq-default indent-tabs-mode nil)     ; indent with spaces instead of tabs
; Use SLIME-style indentation, instead of Emacs' huge tab stops
(setq lisp-indent-function 'common-lisp-indent-function)
(setq standard-indent 4)
(setq c-default-style "ellemtel")
(setq c-basic-offset 2)                 ; imo uses two
(setq js2-basic-offset 4)               ; imo uses four


;; Other settings that 23 broke
(when (>= emacs-major-version 23)
    (setq mac-command-modifier 'meta
          ring-bell-function 'ignore
          split-height-threshold nil))       ; TODO: Not sure this is good -
                                             ; only seems needed in a terminal
                                             ; on Linux
(blink-cursor-mode 1)
(global-set-key (kbd "C-l") 'recenter)  ; recenter-top-bottom? No thanks

;; This doesn't work, but fn-delete should be <delete>, not DEL
;(define-key function-key-map (kbd "<kp-delete>") (kbd "<delete>"))
;; (global-set-key [kp-delete] [delete])
(global-set-key (kbd "<kp-delete>") (kbd "<deletechar>"))

(show-paren-mode t)
; Display
(setq transient-mark-mode t            ; on by default in 23.x
      column-number-mode t
      global-font-lock-mode t
      inhibit-splash-screen t
      truncate-partial-width-windows nil
      ediff-split-window-function 'split-window-horizontally
; Functionality
      require-final-newline 'visit-save ; add on both visit and save
      inferior-erlang-prompt-timeout t
      vc-delete-logbuf-window nil       ; don't close vc window when done
      vc-follow-symlinks t              ; don't always ask
; put the dabbrev (regular M-/ functionality) first
      hippie-expand-try-functions-list '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
          try-complete-file-name-partially try-complete-file-name
          try-expand-all-abbrevs try-expand-list try-expand-line
          try-complete-lisp-symbol-partially try-complete-lisp-symbol)
      ediff-window-setup-function 'ediff-setup-windows-plain ; same window, pls
      mouse-yank-at-point t)            ; middle-click paste at point, not mouse

(setq-default show-trailing-whitespace t
              fill-column 80            ; default of 72 is too narrow
              save-place t)             ; This didn't used to be buffer-local
(put 'upcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)            ; stop forcing me to spell out "yes"
;; TODO: find a way to stop auto-saving files while editing under tramp

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
; when I saw this in vim
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
                            lisp-mode emacs-lisp-mode sh-mode
                            makefile-mode)
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
  (highlight-beyond-fill-column)
  (font-lock-fontify-numbers))

; Add a common hook to every programming mode
(mapc '(lambda (x)
        ; Get the mode's name and turn that into a mode hook
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook 'programming-mode-hook)))
      programming-modes)

(add-hook 'text-mode-hook
          '(lambda ()
            (flyspell-mode t)))

; Trailing whitespace is annoying in some modes
(defvar no-trailing-whitespace-modes '(shell-mode slime-repl-mode))

(mapc '(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook '(lambda ()
                                (setq show-trailing-whitespace nil)))))
      no-trailing-whitespace-modes)


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


;; Modify functions that aren't quite right
(defadvice dired-mark-files-containing-regexp (before unmark-all-first
                                                      (regexp &optional marker-char)
                                                      activate)
  "Unmark marked files in dired mode before searching for new ones"
  (dired-unmark-all-files ?\r))

(defadvice py-shift-region-left (around keep-region-active
                                        (start end &optional count) activate)
  "Keep the region active so we can do multiple shifts"
  (let ((deactivate-mark nil))
    ad-do-it))

(defadvice py-shift-region-right (around keep-region-active
                                         (start end &optional count) activate)
  "Keep the region active so we can do multiple shifts"
  (let ((deactivate-mark nil))
    ad-do-it))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "#ff00ff" :foreground "blue"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "Grey" :foreground "black"))))
 ;; Called both cyan and brightcyan
 '(region ((((class color) (min-colors 24)) (:background "#00ffff")))))


;; Email stuff
(require 'gnus)
(require 'starttls)
(require 'mm-decode)

(setq user-mail-address "msherry@gmail.com")
(setq user-full-name "Marc Sherry")

(setq gnus-select-method '(nnimap "gmail"
                           (nnimap-address "imap.gmail.com")
                           (nnimap-server-port 993)
                           (nnimap-authinfo-file "~/.authinfo")
                           (nnimap-stream ssl))
      gnus-secondary-select-methods '((nnimap "imo"
                                       (nnimap-address "imap.gmail.com")
                                       (nnimap-server-port 993)
                                       (nnimap-authinfo-file "~/.authinfo-imo")
                                       (nnimap-stream ssl)))
      gnus-use-full-window nil)

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)))

(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "msherry@gmail.com" nil))
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(setq smtpmail-local-domain nil)
(setq gnus-permanently-visible-groups "\\(.*INBOX\\|.*Feedback\\)")
(executable-find starttls-program)

;; Default from address for outgoing mail is msherry@gmail.com, but if we detect
;; @imo.im in the headers, use that instead
(defadvice smtpmail-send-it (around google-apps-message-send-mail
                                     protect activate)
  (interactive "P")
  (if (save-restriction
        (message-narrow-to-headers)
        (string-match "imo.im" (message-fetch-field "from")))
      (let ((smtpmail-auth-credentials '(("smtp.gmail.com" 587 "marc@imo.im" nil))))
        ad-do-it)
      ad-do-it))

;; (Require 'pop3)
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnml ""))

;; (setq gnus-posting-styles
;;    '((".*" (name "Marc Sherry"))))

;; (setq mail-sources
;;       '((pop :server "pop.gmail.com"
;; 	     :port 995
;; 	     :user "msherry"
;; 	     :connection ssl
;; 	     :leave t)))

;; (setq gnus-permanently-visible-groups "mail")

;; (setq starttls-use-gnutls t)

;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "msherry@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-local-domain "imo.im")
