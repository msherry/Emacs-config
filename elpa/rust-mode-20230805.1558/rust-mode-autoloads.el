;;; rust-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from rust-cargo.el

(register-definition-prefixes "rust-cargo" '("rust-"))


;;; Generated autoloads from rust-compile.el

(register-definition-prefixes "rust-compile" '("cargo-compilation-regexps" "rustc-"))


;;; Generated autoloads from rust-mode.el

(autoload 'rust-mode "rust-mode" "\
Major mode for Rust code.

\\{rust-mode-map}

(fn)" t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(register-definition-prefixes "rust-mode" '("rust-"))


;;; Generated autoloads from rust-playpen.el

(register-definition-prefixes "rust-playpen" '("rust-"))


;;; Generated autoloads from rust-rustfmt.el

(register-definition-prefixes "rust-rustfmt" '("rust-"))


;;; Generated autoloads from rust-utils.el

(autoload 'rust-dbg-wrap-or-unwrap "rust-utils" "\
Either remove or add the dbg! macro." t)
(register-definition-prefixes "rust-utils" '("rust-"))

;;; End of scraped data

(provide 'rust-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; rust-mode-autoloads.el ends here