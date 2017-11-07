;;; tickscript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tickscript-mode" "tickscript-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from tickscript-mode.el

(autoload 'tickscript-mode "tickscript-mode" "\
Major mode for editing TICKscript files

\\{tickscript-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.tick\\'" . tickscript-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tickscript-mode" '("tickscript-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tickscript-mode-autoloads.el ends here
