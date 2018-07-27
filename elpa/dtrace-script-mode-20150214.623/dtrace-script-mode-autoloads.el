;;; dtrace-script-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dtrace-script-mode" "dtrace-script-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from dtrace-script-mode.el

(autoload 'dtrace-script-mode "dtrace-script-mode" "\
Major mode for editing DTrace code.
This is much like C mode.  Its keymap inherits from C mode's and it has the same
variables for customizing indentation.  It has its own abbrev table and its own
syntax table.
\\{dtrace-script-mode-map}

Turning on DTrace mode runs `dtrace-script-mode-hook'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dtrace-script-mode" '("dtrace-script-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dtrace-script-mode-autoloads.el ends here
