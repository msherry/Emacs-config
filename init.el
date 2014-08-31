;; Much of this is organized similar to:
;; http://bitbucket.org/brodie/dotfiles/src/tip/.emacs

;; Do these early so if there's an error in config we don't pollute ~/ with
;; junk files
(setq save-place-file "~/.emacs.d/emacs-places"
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

; Seed RNG
(random t)

;; While emacs23 handles greek poorly on the mac, use a different font. See
;; last http ref in plugins/lambda.el. "fontset-startup" (or fontsets at all,
;; it seems) don't exist in 22, so only do this on the mac for now
(when (eq system-type 'darwin)
  ;; Default on mac - "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"

  ;; Try this font for a while. Either syntax is acceptable here
  (set-default-font "Inconsolata-14"
                    ;"-apple-Inconsolata-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                    )

  (set-fontset-font t
                    ; used to use 'greek-iso8859-7, but that overwrote more
                    ; than just the lambda character, so now we specify a range of 1
                    ; char
        	    '(955 . 955)
        	    "-apple-Andale_Mono-medium-normal-normal-*-14-*-*-*-p-0-iso10646-1")
;; Use a decent japanese font for all kanji, hiragana, and katakana, rather
;; than a crap chinese font. It seems that it's kind of random when a font is
;; used, and can change, so we specify all three ranges manually. Obviously we
;; could combine hiragana and katakana ranges.
;; これ は 二本語 です
 (mapc '(lambda (x)
         (set-fontset-font "fontset-default"
          x
          "-apple-Osaka-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1"))
       '((?\x3040 . ?\x309F)           ; Hiragana
         (?\x30A0 . ?\x30FF)           ; Katakana
         (?\x4E00 . ?\x9FBF)           ; Kanji
         )))

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
          (set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;   (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  ;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100)))

;; I edit these files a lot, so put them in registers
(set-register ?z '(file . "~/.emacs.d/init.el"))
(set-register ?p '(file . "~/.emacs.d/pass.org.gpg"))
(set-register ?t '(file . "~/TODO.org"))
(set-register ?f '(file . "~/Desktop/fraud/fraud_scripts.py"))

;;; Set the PATH, even if not started from the shell
;;; https://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not
started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; Set up environment
(set-language-environment "UTF-8")

(setq expanded-user-emacs-directory (expand-file-name user-emacs-directory))

;; Plugins - add plugins dir, vendors dir, and all dirs under vendor
;; excluding . and ..
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor")
(dolist (dir (directory-files "~/.emacs.d/plugins/vendor" t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

;; Configure ELPA (package loader)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; Others'
(when (locate-library "auctex")
  (require 'tex-site))
(require 'auto-complete)
(require 'comint)        ; better key handling in shell mode
(require 'doxymacs)
(require 'erlang-start)
(require 'ess)
(require 'flymake-point) ; shows errors in the minibuffer when highlighted
(require 'highlight-beyond-fill-column)
(require 'paredit)
(require 'rainbow-mode)
(require 'saveplace)
(require 'slime-autoloads)
(require 'tramp)
(require 'uniquify)      ; stop naming buffers <2>
(require 'xclip)         ; OMG I love you - now I can copy and paste from linux
(require 'yasnippet)
;; (load "~/.emacs.d/plugins/vendor/nxhtml/autostart.el")
; Mine
(require 'load-edict)
(require 'tags-funcs)
(require 'lisp-customization)
(require 'custom-faces)
(require 'flymake-stuff)
(require 'lambda)
(require 'totd)
(require 'disaster)
(when (< emacs-major-version 23)
  (progn
    (require 'old-emacs-git)
    (require 'vc-svn)))

; Theme
(load-theme 'solarized-dark t)

; Autoloads
(autoload 'js2-mode "js2" nil t)
(autoload 'actionscript-mode "actionscript-mode" nil t) ; Connors' version
(autoload 'php-mode "php-mode" nil t) ; either Mac or 22 only
;; Use python-mode, instead of the crappy built-in python.el on the mac
(autoload 'python-mode "python-mode" "Python Mode." t)
(autoload 'clojure-mode "clojure-mode" "Clojure Mode" t)
(autoload 'turn-on-cldoc-mode "cldoc" "CL docs" t)
(autoload 'jedi-setup-venv "jedi-local" nil t)
(autoload 'jedi:setup "jedi" nil t)

;;; Configure snippets
;; load all el files in the snippets directory, they're usually lisp
;; helpers that help with snippet expansions.
(setq core-custom-snippets (concat expanded-user-emacs-directory "snippets"))
(mapc 'load (directory-files core-custom-snippets t "^[^#].*el$"))
(yas/initialize)
(yas/load-directory (concat expanded-user-emacs-directory "snippets"))

;; Enable preview-latex
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)

;; File/mode associations
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(if (< emacs-minor-version 2)           ; js-mode was made standard in 23.2
    (progn
      (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
      (add-to-list 'auto-mode-alist '("\\.pac$" . js2-mode)))
    (progn
      (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
      (add-to-list 'auto-mode-alist '("\\.pac$" . js-mode))))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; Indentation settings
(setq-default indent-tabs-mode nil)     ; indent with spaces instead of tabs
; Use SLIME-style indentation, instead of Emacs' huge tab stops
(setq lisp-indent-function 'common-lisp-indent-function)
(setq standard-indent 4)
(setq c-default-style "bsd")
(setq c-basic-offset 4)                 ; imo uses four
;; (setq js2-basic-offset 4)               ; imo uses four

(setq mac-pass-command-to-system nil)

;; Settings that 24 broke
;; TODO: figure out why this doesn't work on the mac
(setq-default grep-find-use-xargs 'exec)

;; Other settings that 23 broke
(when (>= emacs-major-version 23)
    (setq mac-command-modifier 'meta
          ring-bell-function 'ignore
          split-height-threshold nil))      ; TODO: Not sure this is good -
                                            ; only seems needed in a terminal
                                            ; on Linux
(blink-cursor-mode 1)
(global-set-key (kbd "C-l") 'recenter)  ; recenter-top-bottom? No thanks

;; This doesn't work, but fn-delete should be <delete>, not DEL
;(define-key function-key-map (kbd "<kp-delete>") (kbd "<delete>"))
;; (global-set-key [kp-delete] [delete])
(global-set-key (kbd "<kp-delete>") (kbd "<deletechar>"))

;; I don't really need a key bound to the GPL
(global-set-key (kbd "C-h C-c") 'hc)

;; Forward-deletion of words
(global-set-key (kbd "M-<kp-delete>") 'kill-word)

(show-paren-mode t)
; Display
(setq transient-mark-mode t            ; on by default in 23.x
      column-number-mode t
      global-font-lock-mode t
      inhibit-splash-screen t
      truncate-partial-width-windows nil
      ediff-split-window-function 'split-window-horizontally
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

(setq-default show-trailing-whitespace t
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
; IBuffer is better than list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

; Better buffer switching
(iswitchb-mode t)
; Don't prompt when creating scratch buffers
(setq iswitchb-prompt-newbuffer nil)

;; ; Trailing newlines are annoying in snippets
;; (setq-default mode-require-final-newline nil)

; Give us the ability to leave certain words highlighted - always got jealous
; when I saw this in vim
(global-hi-lock-mode 1)

; Warn about suspicious C/C++ constructs
(global-cwarn-mode 1)

; Don't complain about utf8 as a coding system name
(define-coding-system-alias 'utf8 'utf-8)

; Just adding this for Jedi for python - no clue what effect it has on other
; languages
(global-auto-complete-mode)

; Create two windows initially if we have the room. Check both current width
; and width from default-frame-alist (if present), since the frame may not have
; been resized yet
(if (> (max (window-width)
            (or (cdr (assoc 'width default-frame-alist))
                0))
       160)
    (split-window-horizontally))


;; Mode hooks
(defvar programming-modes '(python-mode js-mode js2-mode java-mode c-mode
                            objc-mode actionscript-mode lisp-mode
                            emacs-lisp-mode sh-mode makefile-mode conf-mode
                            ruby-mode ess-mode)
  "Modes used for programming")

(defun really-set-keys ()
  "Force our keys even in modes that try to override them"
  (local-set-key (kbd "C-c C-c") 'compile) ;TODO: not in python mode
  (local-set-key (kbd "C-c .") 'flymake-goto-next-error))


(defun programming-mode-hook ()
  "Hook common to all programming modes"
  (add-todo-to-current-mode)
  (flyspell-prog-mode)
  (really-set-keys)
  (highlight-beyond-fill-column)
  (font-lock-fontify-numbers)
  (doxymacs-mode)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Second line of arglists gets two indents
  (c-set-offset 'arglist-cont-nonempty '++)
  (c-set-offset 'arglist-cont '++)
  (c-set-offset 'arglist-close '++))


;; Colors in files where it makes sense
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'dot-mode-hook 'rainbow-mode)

; Add a common hook to every programming mode
(mapc '(lambda (x)
        ; Get the mode's name and turn that into a mode hook
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook 'programming-mode-hook)))
      programming-modes)

(add-hook 'text-mode-hook
          '(lambda ()
            (flyspell-mode t)
            ;(longlines-mode t)
            ))

; Trailing whitespace is annoying in some modes
(defvar no-trailing-whitespace-modes '(shell-mode slime-repl-mode text-mode
                                       fundamental-mode term-mode))

(mapc '(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook '(lambda ()
                                (setq show-trailing-whitespace nil)))))
      no-trailing-whitespace-modes)


(add-hook 'objc-mode-hook
          '(lambda ()
            (setq c-basic-offset 2)))

(add-hook 'python-mode-hook
          '(lambda ()
            ()
            (setq jedi:setup-keys t)
            (jedi-setup-venv)
            (jedi:setup)
            (setq jedi:complete-on-dot t)
            (setq jedi:tooltip-method nil)))

;; Tramp adds a hook to auto-save files. Remove it
(remove-hook 'find-file-hook 'tramp-set-auto-save)
;; That's not really enough to turn off auto-save remotely, so add our own hook
;; here
(require 'custom-utils)
(defun turn-off-auto-save-mode-if-tramp ()
  (when (not (file-is-local-and-writable-p))
    (auto-save-mode nil)))

(add-hook 'find-file-hook 'turn-off-auto-save-mode-if-tramp)

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

(defun bh-choose-header-mode ()
  "Choose the correct c style (Objective-C, C++, C) when opening a .h file, based
on the presence of a similarly-named .m/.cpp file.

Based on http://bretthutley.com/programming/emacs/opening-a-cobjective-cc-header-file-in-emacs/ , but with additional hacks for frameworks by Marc Sherry"
  (interactive)
  (let ((fn (buffer-file-name)))
    (if (string-equal (substring fn -2) ".h")
        (progn
          ;; OK, we got a .h file, if a .m file exists we'll assume it's an
          ;; objective c file. Otherwise, we'll look for a .cpp file.
          (let ((dot-m-file (concat (substring fn 0 -1) "m"))
                (dot-cpp-file (concat (substring fn 0 -1) "cpp")))
            (if (file-exists-p dot-m-file)
                (objc-mode)
                (if (file-exists-p dot-cpp-file)
                    (c++-mode))
                ;; Could be C, or could be Objective-C with no matching .m file
                ;; (e.g., framework headers). Check for the #import directive,
                ;; which is mostly Objective-C (and Microsoft-specific C++).
                (progn
                  (if (with-temp-buffer
                        (insert-file-contents fn)
                        (goto-char (point-min))
                        (re-search-forward "^#import\\|@\"\\|@protocol" nil t))
                      (objc-mode)))))))))
(add-hook 'find-file-hook 'bh-choose-header-mode)

(defun show-whitespace-in-diffs ()
  "Toggle showing whitespace in svn diffs"
  (interactive)
  (setq vc-svn-diff-switches (if (eq vc-svn-diff-switches t)
                                 '("-x" "-w")
                                 t)))


;; Modify functions that aren't quite right
(defadvice dired-mark-files-containing-regexp (before unmark-all-first
                                                      (regexp &optional marker-char)
                                                      activate)
  "Unmark marked files in dired mode before searching for new ones"
  (dired-unmark-all-files ?\r))

(defadvice dired-mark-python-with-errors (before unmark-all-first-2
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


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "#ff00ff" :foreground "blue"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "Grey" :foreground "black"))) t)
 '(error ((t (:background "LightCoral"))))
 '(region ((((class color) (min-colors 24)) (:background "#00ffff"))))
 '(warning ((t (:background "#a4caff")))))


(defun switch-font ()
  "Temporary function to quickly switch between monaco and
inconsolata. Really only applicable on the Mac."
  (interactive)
  (let* ((monaco "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
         (inconsolata "-apple-Inconsolata-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
         (current (cdr (assoc 'font (frame-parameters))))
         (new (if (string= current monaco)
                  inconsolata
                  monaco)))
    (message "New font: %s" new)
    (set-frame-font new)))

(global-set-key (kbd "C-c C-v") 'switch-font)
(put 'narrow-to-region 'disabled nil)

(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes.

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(mumamo-chunk-coloring 999))
(put 'downcase-region 'disabled nil)


(eval-after-load 'cc-mode
  '(progn
     (require 'disaster)
     (defun my-c-mode-common-hook ()
       (define-key c-mode-base-map (kbd "C-c C-d") 'disaster))
    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)))

;; Python/Rope
;; Move this elsewhere
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
