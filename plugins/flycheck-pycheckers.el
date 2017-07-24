;;; flycheck-pycheckers --- flycheck-compatible multiple syntax checker for Python

;; Copyright Marc Sherry <msherry@gmail.com>
;; Package-Requires: ((flycheck "0.18"))

;;; Commentary:

;; This is an alternative way of running multiple Python syntax checkers in
;; flycheck that doesn't depend on flycheck's chaining mechanism.  flycheck is
;; opinionated about what checkers should be run (see
;; https://github.com/flycheck/flycheck/issues/185), and chaining is difficult
;; to get right (e.g. see https://github.com/flycheck/flycheck/issues/836).
;; This package assumes that the user knows what they want, and can configure
;; their checkers accordingly -- if they want to run both flake8 and pylint,
;; that's fine.

;;; Code:
(require 'flycheck)

(flycheck-def-args-var flycheck-pycheckers-args python-pycheckers
  )

(flycheck-def-option-var flycheck-pycheckers-checkers '(pylint mypy2 mypy3) python-pycheckers
    "The set of enabled checkers to run"
  :type '(set (const :tag "pylint" pylint)
              (const :tag "mypy 2" mypy2)
              (const :tag "mypy 3" mypy3)
              (const :tag "PEP8" pep8)
              (const :tag "flake8" flake8)
              (const :tag "pyflakes" pyflakes)
              (const :tag "pydo" pydo)))

(flycheck-def-option-var flycheck-pycheckers-ignore-codes '("C0411"
                                                            "C0413")
    python-pycheckers
  "A list of error codes to ignore"
  :type '(repeat :tag "Codes" (string :tag "Error/Warning code")))

(flycheck-def-option-var flycheck-pycheckers-max-line-length 80
    python-pycheckers
  "The maximum line length allowed by the checkers."
  :type 'integer)

(flycheck-define-checker python-pycheckers
  "Multiple python syntax checker.

You can use `customize' to change the default values used for
every project, and directory-specific `.pycheckers' files to
customize things per-directory."

  :command ("~/.emacs.d/plugins/pycheckers.py" ;TODO: handle empty checker list
            (eval flycheck-pycheckers-args)
            "-i" (eval (mapconcat 'identity flycheck-pycheckers-ignore-codes ","))
            "-c" (eval (mapconcat #'symbol-name flycheck-pycheckers-checkers ","))
            "--max-line-length" (eval (number-to-string flycheck-pycheckers-max-line-length))
            source-original)
  :error-patterns
  ((error line-start
          "ERROR " (optional (id (one-or-more (not (any ":"))))) ":"
          (message) " at " (file-name) " line " line (optional "," column) "." line-end)
   (warning line-start
            "WARNING " (optional (id (one-or-more (not (any ":"))))) ":"
            (message) " at " (file-name) " line " line (optional "," column) "." line-end))
  :modes python-mode)

;;; *Pre*pend this to 'flycheck-checkers, since we want to use this in
;;; *preference to all other checkers
(push 'python-pycheckers flycheck-checkers)


(provide 'flycheck-pycheckers)
;;; flycheck-pycheckers.el ends here
