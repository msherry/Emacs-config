;; Marc Sherry

;; Custom stuff for using Common Lisp - marc
(add-hook 'lisp-mode-hook #'(lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook #'(lambda () (inferior-slime-mode t)))

;; Which Common Lisp do we want to use
;(setq inferior-lisp-program "clisp -K full")
;(setq inferior-lisp-program "cmucl")
(setq inferior-lisp-program "sbcl")


(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook #'cliki:start-slime)

(add-hook 'slime-repl-mode-hook
          #'(lambda ()
            (eldoc-mode nil)
            (paredit-mode)
            (local-set-key (kbd "s-<up>") 'slime-repl-backward-input)
            (local-set-key (kbd "s-<down>") 'slime-repl-forward-input)))

(add-hook 'geiser-repl-mode-hook
          #'(lambda ()
            (paredit-mode)
            (local-set-key (kbd "s-<up>") 'comint-previous-input)
            (local-set-key (kbd "s-<down>") 'comint-next-input)
            ;; Unset the default comint fns
            (local-set-key (kbd "<up>") nil)
            (local-set-key (kbd "<down>") nil)))

;; CLdoc
;; (dolist (hook '(lisp-mode-hook
;;                 slime-repl-mode-hook))
;;   (add-hook hook 'turn-on-cldoc-mode))

;; ElDoc mode for modes that support it
(defvar eldoc-supported-modes '(emacs-lisp-mode))

(mapc #'(lambda (x)
        (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
          (add-hook mode-hook 'turn-on-eldoc-mode)))
      eldoc-supported-modes)

(defun lisp-editing-hook ()
  (paredit-mode)
  (local-set-key (kbd "C-.") #'find-function-at-point))

(defvar lisp-editing-modes '(lisp-mode emacs-lisp-mode scheme-mode
                             clojure-mode geiser-repl-mode cider-repl-mode))

(mapc #'(lambda (x)
          (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
            (add-hook mode-hook #'lisp-editing-hook)))
      lisp-editing-modes)

(eval-after-load "slime"
  '(progn
    (setq slime-contribs '(slime-fancy slime-banner slime-asdf))
    (slime-setup)
    ; That just turned on slime-autodoc, which uses eldoc, which sucks. Turn
    ; off eldoc and use the better built-in(?) doc mode
    (eldoc-mode nil)))

(add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook #'turn-on-eldoc-mode)

(provide 'lisp-customization)
