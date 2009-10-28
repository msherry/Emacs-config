;; Marc Sherry

;; This is retarded :)

;; real lisp hackers use the lambda character
;; courtesy of stefan monnier on c.l.l
(defun sm-lambda-mode-hook ()
  "Stupid hook to turn the word Lambda (lowercase) into λ using
font-lock-mode"
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
   (0 (progn (compose-region (match-beginning 0) (match-end 0)
        ,(make-char 'greek-iso8859-7 107))
      nil))))))

;; Which modes use 'lambda'
(add-hook 'emacs-lisp-mode-hook 'sm-lambda-mode-hook)
(add-hook 'lisp-interactive-mode-hook 'sm-lamba-mode-hook)
(add-hook 'lisp-mode-hook 'sm-lambda-mode-hook)
(add-hook 'slime-repl-mode-hook 'sm-lambda-mode-hook)
(add-hook 'scheme-mode-hook 'sm-lambda-mode-hook)
(add-hook 'python-mode-hook 'sm-lambda-mode-hook)

(provide 'lambda)
