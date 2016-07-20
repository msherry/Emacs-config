;;; ac-geiser-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ac-geiser" "ac-geiser.el" (22260 53456 0 0))
;;; Generated autoloads from ac-geiser.el

(defvar ac-source-geiser '((candidates . ac-source-geiser-candidates) (symbol . "g") (document . ac-geiser-documentation)) "\
Source for geiser completion")

(autoload 'ac-geiser-setup "ac-geiser" "\
Add the geiser completion source to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ac-geiser-pkg.el") (22260 53456 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ac-geiser-autoloads.el ends here
