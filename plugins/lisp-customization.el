;; Marc Sherry

;; Custom stuff for using Common Lisp - marc
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Which Common Lisp do we want to use
;(setq inferior-lisp-program "clisp -K full")
;(setq inferior-lisp-program "cmucl")
(setq inferior-lisp-program "sbcl")


(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'cliki:start-slime)

(add-hook 'slime-repl-mode-hook
          '(lambda ()
            (eldoc-mode nil)
            (paredit-mode)
            (local-set-key (kbd "M-<up>") 'slime-repl-backward-input)
            (local-set-key (kbd "M-<down>") 'slime-repl-forward-input)))

;; CLdoc
;; (dolist (hook '(lisp-mode-hook
;;                 slime-repl-mode-hook))
;;   (add-hook hook 'turn-on-cldoc-mode))

;; ElDoc mode for modes that support it
(defvar eldoc-supported-modes '(emacs-lisp-mode))

(mapc '(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook 'turn-on-eldoc-mode)))
      eldoc-supported-modes)

;; Lisp editing modes
(defun lisp-editing-hook ()
    (progn
      (paredit-mode)))

(defvar lisp-editing-modes '(emacs-lisp-mode clojure-mode))

(mapc '(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook 'lisp-editing-hook)))
      lisp-editing-modes)

(eval-after-load "slime"
  '(progn
    (setq slime-contribs '(slime-fancy slime-banner slime-asdf))
    (slime-setup)
    ; That just turned on slime-autodoc, which uses eldoc, which sucks. Turn
    ; off eldoc and use the better built-in(?) doc mode
    (eldoc-mode nil)))

(provide 'lisp-customization)
