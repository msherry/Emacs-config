;;; ac-geiser-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-geiser" "ac-geiser.el" (0 0 0 0))
;;; Generated autoloads from ac-geiser.el

(defvar ac-source-geiser '((candidates . ac-source-geiser-candidates) (symbol . "g") (document . ac-geiser-documentation)) "\
Source for geiser completion")

(autoload 'ac-geiser-setup "ac-geiser" "\
Add the geiser completion source to the front of `ac-sources'.
This affects only the current buffer." t nil)

(register-definition-prefixes "ac-geiser" '("ac-"))

;;;***

;;;### (autoloads nil nil ("ac-geiser-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-geiser-autoloads.el ends here
