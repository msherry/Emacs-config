;;; salt-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "salt-mode" "salt-mode.el" (0 0 0 0))
;;; Generated autoloads from salt-mode.el

(autoload 'salt-mode "salt-mode" "\
A major mode to edit Salt States.

To view documentation in Emacs or inline with ElDoc, Python and
the Salt Python libraries must be installed on the system
containing the files being edited. (A running minion is not
required.)

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.sls\\'" . salt-mode))

(register-definition-prefixes "salt-mode" '("salt-mode-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; salt-mode-autoloads.el ends here
