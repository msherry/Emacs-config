;;; init.el -- msherry's init.el

;;; Commentary:

;; Much of this is organized similar to:
;; http://bitbucket.org/brodie/dotfiles/src/tip/.emacs

;;; Code:

;; Do these early so if there's an error in config we don't pollute ~/ with
;; junk files
(defvar save-place-file)                ; C-h i g (Elisp) Warning Tips
(setq save-place-file "~/.emacs.d/emacs-places"
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

; Seed RNG
(random t)

;; Plugins - add plugins dir, vendors dir, and all dirs under vendor
;; excluding . and ..
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor")
(add-to-list 'load-path "~/.emacs.d/plugins/language-specific")
(dolist (dir (directory-files "~/.emacs.d/plugins/vendor" t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(when (and (eq system-type 'darwin) window-system)
  (require 'msherry-macos))

;; Set up GUI as soon as possible
(when window-system
  (cond ((eq system-type 'darwin)
         (setq default-frame-alist
               (append `((left . ,(cond ((< emacs-major-version 23) 15)
                                        (t 8)))
                         (top . 20)
                         (width . 175)
                         (height . ,(cond ((< emacs-major-version 23) 47)
                                          ((< emacs-major-version 24) 52)
                                          (t 49))))
                       default-frame-alist)))
        ((eq system-type 'gnu/linux)
         (setq default-frame-alist
               (append `((left . 520)
                         (top . 48)
                         (width . 186)
                         (height . 87))))
         (set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")))
  (if (fboundp #'tool-bar-mode) (tool-bar-mode -1))

  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100)))

;; I edit these files a lot, so put them in registers
(set-register ?a '(file . "~/src/all-the-things"))
(set-register ?r '(file . "~/src/risk-ops"))
(set-register ?z '(file . "~/.emacs.d/init.el"))
(set-register ?p '(file . "~/.emacs.d/pass.org.gpg"))
(set-register ?t '(file . "~/TODO.org"))
(set-register ?f '(file . "~/Desktop/fraud/fraud_scripts.py"))
(set-register ?g '(file . "~/code/pinboard/schemas/goldpick_commons/ttypes.py"))

;; Set up environment
(set-language-environment "UTF-8")

(defvar expanded-user-emacs-directory (expand-file-name user-emacs-directory))

;; Configure ELPA (package loader)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
;; Marmalade is less up-to-date than melpa and is currently giving certificate
;; errors
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; Others'
(when (locate-library "auctex")
  (require 'tex-site))
(require 'auto-complete)
(require 'comint)        ; better key handling in shell mode
;; (require 'doxymacs)
(require 'erlang-start)
(require 'flycheck-pycheckers)
(require 'highlight-beyond-fill-column)
(require 'jabber-keymap) ; This loads inconsistently on its own
(require 'magit)
(require 'paredit)
(require 'rainbow-mode)
(require 'saveplace)
(require 'slime-autoloads)
(require 'tramp)
(require 'uniquify)      ; stop naming buffers <2>
(require 'xclip)         ; OMG I love you - now I can copy and paste from linux
(require 'yasnippet)

; TODO: clean this up, it comes from http://www.braveclojure.com/basic-emacs/
(load "setup-clojure.el")
;; (load "~/.emacs.d/plugins/vendor/nxhtml/autostart.el")
; Mine
(require 'custom-faces)
(when (locate-library "disaster")
  (require 'disaster))
(require 'lambda)
(require 'lisp-customization)
(require 'load-edict)
(require 'msherry-c)
(require 'msherry-go)
(require 'msherry-mail)
(require 'msherry-mirth)
(require 'msherry-python)
(require 'org-customization)
(require 'tags-funcs)
(require 'totd)
(when (version< emacs-version "23")
  (progn
    (require 'old-emacs-git)
    (require 'vc-svn)))

; Theme
(when window-system
  (load-theme 'solarized-dark t))

; Modeline
(setq read-mail-command #'(lambda () (notmuch)))

; Autoloads
(autoload 'actionscript-mode "actionscript-mode" nil t) ; Connors' version
(autoload 'clojure-mode "clojure-mode" "Clojure Mode" t)
(autoload 'csv-mode "csv-mode" nil t)
(autoload 'ess-mode "ess-mode" "ESS mode" t)
(autoload 'js2-mode "js2" nil t)
(autoload 'php-mode "php-mode" nil t) ; either Mac or 22 only
(autoload 'turn-on-cldoc-mode "cldoc" "CL docs" t)

;;; Configure snippets
;; load all el files in the snippets directory, they're usually lisp
;; helpers that help with snippet expansions.
(setq core-custom-snippets (concat expanded-user-emacs-directory "snippets"))
(mapc 'load (directory-files core-custom-snippets t "^[^#].*el$"))
(yas-global-mode 1)
(yas-load-directory (concat expanded-user-emacs-directory "snippets"))

(auto-insert-mode 1)

;; Enable preview-latex
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)

;; File/mode associations
(add-to-list 'auto-mode-alist '("BUILD$" . bazel-mode))
(add-to-list 'auto-mode-alist '("\\.R$" . r-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.bzl$" . bazel-mode))
(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.d$" . dtrace-script-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.ml$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.pyst$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . js2-mode))


;; Indentation settings
(setq-default indent-tabs-mode nil)     ; indent with spaces instead of tabs
; Use SLIME-style indentation, instead of Emacs' huge tab stops
(setq lisp-indent-function #'common-lisp-indent-function)
(setq standard-indent 4)
(setq c-default-style "bsd")
(setq c-basic-offset 4)                 ; imo uses four
(setq js2-basic-offset 2)               ; imo uses four

;; Settings that 24 broke
;; TODO: figure out why this doesn't work on the mac
(setq-default grep-find-use-xargs 'exec)

;; Other settings that 23 broke
(when (>= emacs-major-version 23)
    (setq ring-bell-function 'ignore
          split-height-threshold nil))      ; TODO: Not sure this is good -
                                            ; only seems needed in a terminal
                                            ; on Linux
(blink-cursor-mode 1)
(show-paren-mode t)
(ido-ubiquitous-mode 1)
(global-emojify-mode)
(which-key-mode)

; Display
(setq transient-mark-mode t            ; on by default in 23.x
      column-number-mode t
      global-font-lock-mode t
      inhibit-splash-screen t
      truncate-partial-width-windows nil
      ediff-split-window-function #'split-window-horizontally
; Functionality
      require-final-newline t           ; add only on save
      inferior-erlang-prompt-timeout t
      vc-delete-logbuf-window nil       ; don't close vc window when done
      vc-follow-symlinks t              ; don't always ask
      vc-log-show-limit 0               ; I like seeing the full log

; put the dabbrev (regular M-/ functionality) first
      hippie-expand-try-functions-list '(
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
          try-complete-file-name-partially try-complete-file-name
          try-expand-all-abbrevs try-expand-list try-expand-line
          try-complete-lisp-symbol-partially try-complete-lisp-symbol)
      ediff-window-setup-function 'ediff-setup-windows-plain ; same window, pls
      mouse-yank-at-point t)            ; middle-click paste at point, not mouse

(setq-default show-trailing-whitespace nil
              fill-column 79            ; default of 72 is too narrow
              save-place t)             ; This didn't used to be buffer-local
(put 'upcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)            ; stop forcing me to spell out "yes"

; Uniquifying
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Copy files between dired buffers easily
(setq dired-dwim-target t)

;; Magit settings
(setq magit-popup-show-common-commands t)
(add-to-list 'git-commit-style-convention-checks
             'overlong-summary-line)

;; Bindings
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-l") #'recenter)  ; recenter-top-bottom? No thanks
(global-set-key (kbd "<kp-delete>") (kbd "<deletechar>"))
;; I don't really need a key bound to the GPL
(global-set-key (kbd "C-h C-c") #'hc)
;; Forward-deletion of words
(global-set-key (kbd "M-<kp-delete>") #'kill-word)
;; Magit
(global-set-key (kbd "C-x g") #'magit-status)

(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "C-c e") #'ediff-buffers)
(global-set-key [insertchar] nil)       ; Right next to delete!!
(when window-system
  (global-set-key (kbd "M-`") nil)) ; disable text mode menubar key if not in text mode
; scroll without moving point
(global-set-key (kbd "M-n") (lambda() (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda() (interactive) (scroll-down 1)))
(global-set-key (kbd "C-x \\") #'align-regexp)
(global-set-key (kbd "M-/") #'hippie-expand) ; better than dabbrev
(global-set-key (kbd "C-M-z") #'ack)

;; Unbindings
;;; Mac keyboards make C-z too easy to hit, suspending emacs
(global-unset-key (kbd "C-z"))

;; From http://doc.norang.ca/org-mode.html
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

;; Code browsing
(global-set-key (kbd "C-c C-'") #'mirth)


; Readline in shell mode
(define-key comint-mode-map [up] #'comint-previous-input)
(define-key comint-mode-map [down] #'comint-next-input)
;;; On Mac OS, Ctrl-(arrow) changes desktops, so rebind these for paredit
(define-key paredit-mode-map (kbd "s-<right>") #'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "s-<left>") #'paredit-forward-barf-sexp)
;; C-j is useful in scratch buffers, and I don't use the paredit version
(define-key paredit-mode-map (kbd "C-j") nil)
;; Make kill-ring-save work like normal in magit modes
(define-key magit-mode-map (kbd "M-w") nil)

; Util functions for dired
(eval-after-load "dired"
  (lambda ()
    (define-key dired-mode-map "F" #'find-matching-pattern-under-dir)))
; IBuffer is better than list-buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)

; Better buffer switching
(iswitchb-mode t)
; Don't prompt when creating scratch buffers
(setq iswitchb-prompt-newbuffer nil)

;; ; Trailing newlines are annoying in snippets
;; (setq-default mode-require-final-newline nil)

; Give us the ability to leave certain words highlighted - always got jealous
; when I saw this in vim
(global-hi-lock-mode 1)

; Don't complain about utf8 as a coding system name
(define-coding-system-alias 'utf8 'utf-8)

; Just adding this for Jedi for python - no clue what effect it has on other
; languages
(global-auto-complete-mode)

; Let's try flycheck instead of flymake, with a custom checker for our own
; wrapper script
(global-flycheck-mode 1)
;;; TODO: flymake-add-next-checker should let us chain existing checkers
;;; without needing our script at all -
;;; https://github.com/flycheck/flycheck/issues/185
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook
            '(lambda ()
              (local-set-key (kbd "C-c .") 'flycheck-next-error)
              (flycheck-pycheckers-setup)
              (flycheck-rust-setup))
            )
  ;; https://github.com/flycheck/flycheck/issues/1397
;;   (defun flycheck-fill-and-expand-error-file-names (errors directory)
;;   "Fill and expand file names in ERRORS relative to DIRECTORY.

;; Expand all file names of ERRORS against DIRECTORY.  If the file
;; name of an error is nil fill in the result of function
;; `buffer-file-name' in the current buffer.

;; Return ERRORS, modified in-place."
;;   (seq-do (lambda (err)
;;             (setf (flycheck-error-filename err)
;;                   (buffer-file-name)))
;;           errors)
;;   errors)
  )

; Fast jumps to windows
(window-numbering-mode)

; Create two windows initially if we have the room. Check both current width
; and width from default-frame-alist (if present), since the frame may not have
; been resized yet
(if (> (max (window-width)
            (or (cdr (assoc 'width default-frame-alist))
                0))
       160)
    (split-window-horizontally))


;; Mode hooks
(defvar programming-modes '(actionscript-mode c-mode conf-mode emacs-lisp-mode ess-mode
                            go-mode java-mode js-mode js2-mode lisp-mode makefile-mode
                            objc-mode python-mode ruby-mode rust-mode sh-mode tickscript-mode)
  "Modes used for programming.")


(defun programming-mode-hook ()
  "Hook common to all programming modes."
  (msherry-font-lock-fontify-todo)
  (msherry-font-lock-fontify-numbers)
  (flyspell-prog-mode)
  ;; (highlight-beyond-fill-column)
  ;; (doxymacs-mode)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Second line of arglists gets two indents
  (c-set-offset 'arglist-cont-nonempty '++)
  (c-set-offset 'arglist-cont '++)
  (c-set-offset 'arglist-close '++)
  (setq show-trailing-whitespace t)
  ;; Completion that doesn't compete with OSX's program switching
  (local-set-key (kbd "s-<tab>") 'complete-symbol))


;; Colors in files where it makes sense
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'dot-mode-hook 'rainbow-mode)

; Add a common hook to every programming mode
(mapc #'(lambda (x)
        ; Get the mode's name and turn that into a mode hook
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook 'programming-mode-hook)))
      programming-modes)

(add-hook 'text-mode-hook
          #'(lambda ()
            (flyspell-mode t)
            ;(longlines-mode t)
            ))

(add-hook 'elpy-mode-hook
          '(lambda ()
            ;; C-c C-p is now used by projectile, unbind it from
            ;; elpy-flymake-previous-error, but only if elpy is in use
            (when (boundp 'elpy-mode-map)
              (define-key elpy-mode-map (kbd "C-c C-p") nil))))

;;; I can't keep up with every projectile release changing the way it's activated
(add-hook 'projectile-mode-hook
          #'(lambda ()
              (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)))

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'racer-mode)
;;; At some point, rustfmt stopped respecting .rustfmt.toml files. I can't
;;; track down where, so hack it up with a hardcoded path for now.
(add-hook 'rust-mode-hook
          '(lambda ()
            (setq rust--format-args '("--config-path" "/Users/msherry/src/client/rust/nucleus/.rustfmt.toml"))
            (defun rust--format-call (buf)
              "Format BUF using rustfmt."
              (with-current-buffer (get-buffer-create "*rustfmt*")
                (erase-buffer)
                (insert-buffer-substring buf)
                (let* ((tmpf (make-temp-file "rustfmt"))
                       (ret (apply #'call-process-region (point-min) (point-max) rust-rustfmt-bin
                                   t `(t ,tmpf) nil rust--format-args)))
                  (unwind-protect
                       (cond
                         ((zerop ret)
                          (if (not (string= (buffer-string)
                                            (with-current-buffer buf (buffer-string))))
                              (copy-to-buffer buf (point-min) (point-max)))
                          (kill-buffer))
                         ((= ret 3)
                          (if (not (string= (buffer-string)
                                            (with-current-buffer buf (buffer-string))))
                              (copy-to-buffer buf (point-min) (point-max)))
                          (erase-buffer)
                          (insert-file-contents tmpf)
                          (error "Rustfmt could not format some lines, see *rustfmt* buffer for details"))
                         (t
                          (erase-buffer)
                          (insert-file-contents tmpf)
                          (error "Rustfmt failed, see *rustfmt* buffer for details"))))
                  (delete-file tmpf))))))

;; Know what's useless? A lot of flyspell keybindings
(eval-after-load "flyspell"
  '(progn
    (define-key flyspell-mode-map (kbd "C-.") nil)
    (define-key flyspell-mode-map (kbd "C-,") nil)))


;; Tramp adds a hook to auto-save files. Remove it
(remove-hook 'find-file-hook 'tramp-set-auto-save)
;; That's not really enough to turn off auto-save remotely, so add our own hook
;; here
(require 'custom-utils)
(defun turn-off-auto-save-mode-if-tramp ()
  "Disable `auto-save-mode' if the file is remote."
  (when (not (file-is-local-and-writable-p))
    (auto-save-mode nil)))

(add-hook 'find-file-hook 'turn-off-auto-save-mode-if-tramp)

;; Don't check tramp files for VC mode - from
;; http://emacs.1067599.n8.nabble.com/mention-how-to-eliminate-Checking-vc-registered-completely-td467410.html
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Mouse wheel scrolling in xterm
(unless window-system
  (xterm-mouse-mode 1)
  ;(mouse-wheel-mode 1) ;; https://github.com/eschulte/emacs24-starter-kit/issues/32
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
  "Unmark marked files in dired mode before searching for new ones."
  (dired-unmark-all-files ?\r))

(defadvice dired-mark-python-with-errors (before unmark-all-first-2
                                                      (regexp &optional marker-char)
                                                      activate)
  "Unmark marked files in dired mode before searching for new ones."
  (dired-unmark-all-files ?\r))


;; Emacs server
(server-start)
;; Don't prompt re: killing buffers with clients open
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


;; Replace functions that are completely wrong

(eval-after-load "vc-svn"
  '(progn
    (defun vc-svn-annotate-command (file buf &optional rev)
      "Marc's version of this - exactly the same as stock, but 'async
has been replaced with t -- without this, we only get partial
annotations"
      (vc-svn-command buf 't file "annotate" (if rev (concat "-r" rev))))))


(eval-after-load "vc-hg"
  '(progn
    (defun vc-hg-annotate-command (file buffer &optional revision)
      "Marc's version of vc-hg-annotate-command. Adds the -u option"
      (vc-hg-command buffer 0 file "annotate" "-u" "-d" "-n"
                     (when revision (concat "-r" revision))))
    ;; Since we modified the annotate command, we have to retell vc how to
    ;; get the version information. This is dumb.
    ;; Format:
    ;; username (VERSION_NUMBER) (DATE) (FILENAME)/NOTHING: CONTENTS
    (defconst vc-hg-annotate-re
      "^[ \t]*[^ ]+ +\\([0-9]+\\) \\(.\\{30\\}\\)\\(?:\\(: \\)\\|\\([^:]+\\): \\)")
      ;; "^[ \t]*\\([0-9]+\\) \\(.\\{30\\}\\)\\(?:\\(: \\)\\|\\(?: +\\(.+\\): \\)\\)")

    ;; (defun vc-hg-annotate-time ()
    ;;   (when (looking-at vc-hg-annotate-re)
    ;;     (goto-char (match-end 0))
    ;;     (vc-annotate-convert-time
    ;;      (date-to-time (match-string-no-properties 3)))))

    ;; (defun vc-hg-annotate-extract-revision-at-line ()
    ;;   (save-excursion
    ;;     (beginning-of-line)
    ;;     (when (looking-at vc-hg-annotate-re)
    ;;       (if (match-beginning 4)
    ;;           (match-string-no-properties 2)
    ;;           (cons (match-string-no-properties 2)
    ;;                 (expand-file-name (match-string-no-properties 5)
    ;;                                   (vc-hg-root default-directory)))))))
))


(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(defun buffer-mode-histogram ()
  "Display a histogram of Emacs buffer modes.

http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/"
  (interactive)
  (let* ((totals `())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test `equal)))
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                      total-buffers (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))

;; http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;;; Use an external custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; (eval-after-load 'cc-mode
;;   '(progn
;;      (defun my-c-mode-common-hook ()
;;        (define-key c-mode-base-map (kbd "C-c C-d") 'disaster))
;;     (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)))

(byte-recompile-directory expanded-user-emacs-directory)

;; Mitigate Bug#28350 (security) in Emacs 25.2 and
;; earlier. http://www.openwall.com/lists/oss-security/2017/09/11/1
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
    (list start end)))


;;; Snippet for auto-scping a file on save
; (add-hook 'after-save-hook (lambda () (shell-command (format "scp %s apogee-influxdb:kapacitor-tools/" (buffer-name)))) nil t)

;;; Set the PATH, even if not started from the shell.  Formerly did this
;;; ourselves, replaced with exec-path-from-shell. Must be done after the call
;;; to `custom-set-variables' has set things for us.
(exec-path-from-shell-initialize)

;;; This has to be done after we set our projectile-mode-hook, so we do it here
;;; rather than in custom-set-variables
(projectile-mode t)

;;; Misc

(defvar msherry-odin "vegas")
(defvar msherry-raven "raven-periwinkle")

(defun msherry/open-this-file-other-host (host pathspec)
  "Open the current file (via TRAMP) on the given HOST, using PATHSPEC to construct a remote path."
  (let* ((path (buffer-file-name))
         (pos (point))
         (repo-root (vc-root-dir))
         (repo-name (file-relative-name repo-root (file-name-directory (directory-file-name repo-root))))
         (relative-path (file-relative-name path repo-root))
         (remote-path (format pathspec host repo-name relative-path)))
    (find-file-other-window remote-path)
    (goto-char pos)))


(defun open-this-file-on-raven ()
  "Open the current file (via TRAMP) on raven whose name is given by `msherry-raven'."
  (interactive)
  (msherry/open-this-file-other-host msherry-raven "/%s:%s%s"))

(defun open-this-file-on-odin ()
  "Open the current file (via TRAMP) on an ODIN whose name is given by `msherry-odin'."
  (interactive)
  (msherry/open-this-file-other-host msherry-odin "/%s.odin.aff:/opt/code/frontend/%s%s"))

;;; Walk down directory hierarchies when processing dir-locals.el so they can
;;; nest. From https://emacs.stackexchange.com/a/5537/7169
(defun file-name-directory-nesting-helper (name previous-name accumulator)
  (if (string= name previous-name)
      accumulator                       ; stop when names stop changing (at the top)
      (file-name-directory-nesting-helper
       (directory-file-name (file-name-directory name))
       name
       (cons name accumulator))))

(defun file-name-directory-nesting (name)
  (file-name-directory-nesting-helper (expand-file-name name) "" ()))

(defun hack-dir-local-variables-chained-advice (orig)
  "Apply dir-local settings from the whole directory hierarchy, from the top down."
  (let ((original-buffer-file-name (buffer-file-name))
        (nesting (file-name-directory-nesting (or (buffer-file-name)
                                                  default-directory))))
    (unwind-protect
        (dolist (name nesting)
          ;; make it look like we're in a directory higher up in the
          ;; hierarchy; note that the file we're "visiting" does not
          ;; have to exist
          (setq buffer-file-name (expand-file-name "ignored" name))
          (funcall orig))
      ;; cleanup
      (setq buffer-file-name original-buffer-file-name))))

(advice-add 'hack-dir-local-variables :around
            #'hack-dir-local-variables-chained-advice)


(defun isort-buffer ()
  "Apply isort to the current (Python) buffer, with sane defaults."
  (interactive)
  (let ((command
         (mapconcat 'identity
                    `("isort"
                      "--combine-as"
                      "--dont-skip __init__.py" ; why would you skip this?
                      "--line-width 100"
                      "--multi-line 3"
                      "--project affirm"
                      "--project tests"
                      ;; "--thirdparty typing" ; treat this as third-party since we use Python 2.7
                      "--trailing-comma"
                      "--use-parentheses"
                      ,(let ((venv_root
                              (or (bound-and-true-p jedi:environment-root)
                                  (bound-and-true-p python-shell-virtualenv-root)
                                  (getenv "VIRTUAL_ENV"))))
                         (when venv_root
                           (format "--virtual-env %s" venv_root)))
                      "%s")
                    " ")))
    (shell-command (format
                    command
                    (buffer-file-name)))))

(defun activate-monolith-venv ()
  "Activate the monolith's venv"
  (interactive)
  (pyvenv-activate "/Users/marcsherry/src/all-the-things/deployable/monolith/src/.venv"))

(defun activate-fraud-venv ()
  "Activate fraud's venv"
  (interactive)
  (pyvenv-activate "/Users/marcsherry/src/all-the-things/deployable/fraud/src/.venv"))

(defun activate-riskops-venv ()
  "Activate the riskops venv"
  (interactive)
  (pyvenv-activate "/Users/marcsherry/src/risk-ops/riskops/.venv"))


(defun urldecode-region (start end)
  (interactive "r")
  (if (use-region-p)
      (url-unhex-string (buffer-substring start end))))


;;; Enable fuzzy completion for projectile-mode (among others, probably). Found
;;; at https://github.com/bbatsov/projectile/issues/564#issuecomment-65890252
(flx-ido-mode)

(setq ghub-use-workaround-for-emacs-bug 'force)


(provide 'init)

;;; init.el ends here
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
