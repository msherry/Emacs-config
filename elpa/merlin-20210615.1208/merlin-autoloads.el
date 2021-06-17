;;; merlin-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "merlin" "merlin.el" (0 0 0 0))
;;; Generated autoloads from merlin.el

(autoload 'merlin-mode "merlin" "\
Minor mode for interacting with a merlin process.
Runs a merlin process in the background and perform queries on it.

If called interactively, toggle `Merlin mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Short cuts:
\\{merlin-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "merlin" '("merlin-"))

;;;***

;;;### (autoloads nil "merlin-cap" "merlin-cap.el" (0 0 0 0))
;;; Generated autoloads from merlin-cap.el

(register-definition-prefixes "merlin-cap" '("merlin-c"))

;;;***

;;;### (autoloads nil "merlin-imenu" "merlin-imenu.el" (0 0 0 0))
;;; Generated autoloads from merlin-imenu.el

(autoload 'merlin-use-merlin-imenu "merlin-imenu" "\
Merlin: use the custom imenu feature from Merlin" t nil)

(register-definition-prefixes "merlin-imenu" '("merlin-imenu-"))

;;;***

;;;### (autoloads nil "merlin-xref" "merlin-xref.el" (0 0 0 0))
;;; Generated autoloads from merlin-xref.el

(autoload 'merlin-xref-backend "merlin-xref" "\
Merlin backend for Xref." nil nil)

(register-definition-prefixes "merlin-xref" '("merlin-xref--line"))

;;;***

;;;### (autoloads nil nil ("merlin-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; merlin-autoloads.el ends here
