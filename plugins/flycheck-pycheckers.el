

;;; Code:
(require 'flycheck)

(flycheck-def-args-var flycheck-python-pycheckers-args python-pycheckers
  )

(flycheck-define-checker python-pycheckers
  "Multiple python syntax checker.

Customize `flycheck-python-pycheckers-args` to add specific args to default
executable."

  :command ("~/.emacs.d/plugins/pycheckers.py"
            (eval flycheck-python-pycheckers-args)
            source-original)
  :error-patterns
  ((error line-start
          "ERROR " (optional (id (one-or-more (not (any ":"))))) ":"
          (message) " at " (file-name) " line " line "." line-end)
   (warning line-start
            "WARNING " (optional (id (one-or-more (not (any ":"))))) ":"
            (message) " at " (file-name) " line " line "." line-end))
  :modes python-mode)

;;; *Pre*pend this to 'flycheck-checkers, since we want to use this in
;;; *preference to all other checkers
(push 'python-pycheckers flycheck-checkers)


(provide 'flycheck-pycheckers)
;;; flycheck-pycheckers.el ends here
