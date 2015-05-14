;; Marc Sherry

;; This is retarded :)

(defvar min-prettify-emacs-version "24999.4")

;;; Note: on OS X, conflicts in the font we use (andale mono) can cause this to
;;; break. Using OS X's FontBook's 'Resolve Automatically' button fixed the
;;; issue.

;; real lisp hackers use the lambda character
;; courtesy of stefan monnier on c.l.l
(defun sm-lambda-mode-hook ()
  "Stupid hook to turn the word Lambda (lowercase) into λ using
font-lock-mode"
  (if (version< emacs-version min-prettify-emacs-version)
      (progn
        (font-lock-add-keywords
         nil `(("\\<lambda\\>"
                (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                          ,(make-char 'greek-iso8859-7 107))
                          nil))))))
      (progn
        ;; Requires prettify-symbols-mode, which only became available in 24.4
        (push '("lambda" . ?λ) prettify-symbols-alist)
        (prettify-symbols-mode 1))))

(mapc '(lambda (hook)
        (progn
          (add-hook hook 'sm-lambda-mode-hook)))
      '(emacs-lisp-mode-hook lisp-interactive-mode-hook lisp-mode-hook slime-repl-mode-hook scheme-mode-hook python-mode-hook diff-mode-hook))

(provide 'lambda)

;; Resources for trying to fix lambda problem on Mac emacs 23:
;; UPDATE - it's an emacs bug. Sigh...
;; http://www.emacswiki.org/emacs/SetFonts
;; http://3e8.org/zb/quack-pretty-lambda.html
;; http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=3174
