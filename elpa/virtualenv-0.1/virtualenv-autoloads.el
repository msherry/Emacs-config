;;; virtualenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (virtualenv-minor-mode virtualenv-workon) "virtualenv"
;;;;;;  "virtualenv.el" (20313 9298))
;;; Generated autoloads from virtualenv.el

(autoload 'virtualenv-workon "virtualenv" "\
Activate a virtual environment for python.
Optional argument ENV if non-nil, either use the string given as
the virtual environment or if not a string then query the user.

\(fn &optional ENV)" t nil)

(autoload 'virtualenv-minor-mode "virtualenv" "\
Toggle Virtualenv minor mode on or off.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.
\\{virtualenv-minor-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("virtualenv-pkg.el") (20313 9298 625705))

;;;***

(provide 'virtualenv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; virtualenv-autoloads.el ends here
