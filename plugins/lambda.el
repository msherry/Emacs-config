;; Marc Sherry

;; This is retarded :)

;; real lisp hackers use the lambda character
;; courtesy of stefan monnier on c.l.l
(defun sm-lambda-mode-hook ()
  "Stupid hook to turn the word Lambda (lowercase) into λ using
font-lock-mode"
  (if (version< emacs-version "29.4")
      (progn
        (font-lock-add-keywords
         nil `(("\\<lambda\\>"
                (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                          ,(make-char 'greek-iso8859-7 107))
                          nil))))))
      (progn
        ;; Supposed to work in 24.4, requires prettify-symbols-mode
        (push '("lambda" . ?λ) prettify-symbols-alist))))

;; Which modes use 'lambda'
(add-hook 'emacs-lisp-mode-hook 'sm-lambda-mode-hook)
(add-hook 'lisp-interactive-mode-hook 'sm-lamba-mode-hook)
(add-hook 'lisp-mode-hook 'sm-lambda-mode-hook)
(add-hook 'slime-repl-mode-hook 'sm-lambda-mode-hook)
(add-hook 'scheme-mode-hook 'sm-lambda-mode-hook)
(add-hook 'python-mode-hook 'sm-lambda-mode-hook)
(add-hook 'diff-mode-hook 'sm-lambda-mode-hook)

(provide 'lambda)

;; Resources for trying to fix lambda problem on Mac emacs 23:
;; UPDATE - it's an emacs bug. Sigh...
;; http://www.emacswiki.org/emacs/SetFonts
;; http://3e8.org/zb/quack-pretty-lambda.html
;; http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=3174
