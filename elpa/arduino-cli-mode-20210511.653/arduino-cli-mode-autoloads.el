;;; arduino-cli-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "arduino-cli-mode" "arduino-cli-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from arduino-cli-mode.el

(autoload 'arduino-cli-mode "arduino-cli-mode" "\
Arduino-cli integration for Emacs.

If called interactively, toggle `arduino-cli mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "arduino-cli-mode" '("arduino-cli-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; arduino-cli-mode-autoloads.el ends here
