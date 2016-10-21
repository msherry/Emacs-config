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
(when (and (eq system-type 'darwin) window-system)
  (require 'msherry/macos))

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
  (if (fboundp #'tool-bar-mode) (tool-bar-mode -1))
;;   (if (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))

  ;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100)))

;; I edit these files a lot, so put them in registers
(set-register ?z '(file . "~/.emacs.d/init.el"))
(set-register ?p '(file . "~/.emacs.d/pass.org.gpg"))
(set-register ?t '(file . "~/TODO.org"))
(set-register ?f '(file . "~/Desktop/fraud/fraud_scripts.py"))
(set-register ?g '(file . "~/code/pinboard/schemas/goldpick_commons/ttypes.py"))

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

;; TODO: user-emacs-directory depends on emacs >=23
(setq expanded-user-emacs-directory (expand-file-name user-emacs-directory))

;; Plugins - add plugins dir, vendors dir, and all dirs under vendor
;; excluding . and ..
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/vendor")
(add-to-list 'load-path "~/.emacs.d/plugins/language-specific")
(dolist (dir (directory-files "~/.emacs.d/plugins/vendor" t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

;; Configure ELPA (package loader)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

; Others'
(when (locate-library "auctex")
  (require 'tex-site))
(require 'auto-complete)
(require 'comint)        ; better key handling in shell mode
;; (require 'doxymacs)
(require 'erlang-start)
(require 'flymake-point) ; shows errors in the minibuffer when highlighted
(require 'highlight-beyond-fill-column)
(require 'magit)
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
(require 'custom-faces)
(when (locate-library "disaster")
  (require 'disaster))
(require 'flymake-stuff)
(require 'lambda)
(require 'lisp-customization)
(require 'load-edict)
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

;; Enable preview-latex
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)

;; File/mode associations
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))
(if (version< emacs-version "23.2")     ; js-mode was made standard in 23.2
    (progn
      (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
      (add-to-list 'auto-mode-alist '("\\.pac$" . js2-mode)))
    (progn
      (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
      (add-to-list 'auto-mode-alist '("\\.pac$" . js-mode))))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.R$" . r-mode))


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
; scroll without moving point
(global-set-key (kbd "M-n") (lambda() (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda() (interactive) (scroll-down 1)))
(global-set-key (kbd "C-x \\") #'align-regexp)
(global-set-key (kbd "M-/") #'hippie-expand) ; better than dabbrev
(global-set-key (kbd "C-M-z") #'ack)
; Readline in shell mode
(define-key comint-mode-map [up] #'comint-previous-input)
(define-key comint-mode-map [down] #'comint-next-input)
;;; On Mac OS, Ctrl-(arrow) changes desktops, so rebind these for paredit
(define-key paredit-mode-map (kbd "s-<right>") #'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "s-<left>") #'paredit-forward-barf-sexp)

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
(defvar programming-modes '(python-mode js-mode js2-mode java-mode c-mode
                            objc-mode actionscript-mode lisp-mode
                            emacs-lisp-mode sh-mode makefile-mode conf-mode
                            ruby-mode ess-mode)
  "Modes used for programming")


(defun programming-mode-hook ()
  "Hook common to all programming modes"
  (add-todo-to-current-mode)
  (flyspell-prog-mode)
  ;; (highlight-beyond-fill-column)
  (font-lock-fontify-numbers)
  ;; (doxymacs-mode)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Second line of arglists gets two indents
  (c-set-offset 'arglist-cont-nonempty '++)
  (c-set-offset 'arglist-cont '++)
  (c-set-offset 'arglist-close '++)
  (cwarn-mode))


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

;; Know what's useless? A lot of flyspell keybindings
(eval-after-load "flyspell"
  '(progn
    (define-key flyspell-mode-map (kbd "C-.") nil)
    (define-key flyspell-mode-map (kbd "C-,") nil)))

; Trailing whitespace is annoying in some modes
(defvar no-trailing-whitespace-modes '(shell-mode slime-repl-mode text-mode
                                       fundamental-mode term-mode vc-git-log-view-mode
                                       calendar-mode magit-popup-mode
                                       fxrd-mode))

(mapc '(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook '(lambda ()
                                (setq show-trailing-whitespace nil)))))
      no-trailing-whitespace-modes)


(add-hook 'objc-mode-hook
          '(lambda ()
            (setq c-basic-offset 2)))

(require 'java-settings)

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

Based on
http://bretthutley.com/programming/emacs/opening-a-cobjective-cc-header-file-in-emacs/,
but with additional hacks for frameworks by Marc Sherry"
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(button ((t (:foreground "#b58900" :underline t))))
 '(csv-separator-face ((t (:foreground "cornflower blue"))))
 '(diff-added ((t (:background "#002b36" :foreground "green4"))))
 '(diff-refine-added ((t (:inherit diff-refine-change :background "dark green"))))
 '(diff-refine-removed ((t (:inherit diff-refine-change :background "red4"))))
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "#ff00ff" :foreground "blue"))))
 '(ediff-current-diff-C ((t (:background "#888833" :foreground "black"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "Grey" :foreground "black"))))
 '(error ((t (:background "LightCoral"))))
 '(region ((((class color) (min-colors 24)) (:background "#00ffff"))))
 '(warning ((t (:background "#a4caff")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(gc-cons-threshold 100000000)
 '(git-commit-summary-max-length 79)
 '(global-eldoc-mode nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate")))
 '(magit-push-always-verify nil)
 '(magit-tag-arguments (quote ("--annotate")))
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3)))
 '(org-agenda-span (quote day))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-sticky t)
 '(package-selected-packages
   (quote
    (markdown-preview-mode elpy puppet-mode latex-preview-pane fxrd-mode ac-geiser geiser window-numbering json-mode gitignore-mode esup feature-mode zenburn-theme yasnippet yaml-mode thrift solarized-theme slime sass-mode s rainbow-mode pymacs paredit org markdown-mode jedi httpcode go-mode flymake-sass flymake ess diff-hl debbugs clojure-mode ack))))


;; (eval-after-load 'cc-mode
;;   '(progn
;;      (defun my-c-mode-common-hook ()
;;        (define-key c-mode-base-map (kbd "C-c C-d") 'disaster))
;;     (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)))
