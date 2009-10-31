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
            (local-set-key (kbd "M-<up>") 'slime-repl-backward-input)
            (local-set-key (kbd "M-<down>") 'slime-repl-forward-input)
            (cliki:start-slime)))

;; need this on the Mac - maybe it's a 22.x thing?
(if (eq window-system 'mac)
    (eval-after-load "slime"
      '(slime-setup '(slime-fancy slime-banner))))

(provide 'lisp-customization)
