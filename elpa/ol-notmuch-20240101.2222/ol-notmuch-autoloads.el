;;; ol-notmuch-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from ol-notmuch.el

(with-eval-after-load 'org (org-link-set-parameters "notmuch" :store #'org-notmuch-store-link :follow #'org-notmuch-open))
(autoload 'org-notmuch-store-link "ol-notmuch" "\
Store a link to one or more notmuch messages.")
(autoload 'org-notmuch-open "ol-notmuch" "\
Follow a notmuch message link specified by PATH.

(fn PATH _)")
(with-eval-after-load 'org (org-link-set-parameters "notmuch-search" :store #'org-notmuch-search-store-link :follow #'org-notmuch-search-open))
(autoload 'org-notmuch-search-store-link "ol-notmuch" "\
Store a link to a notmuch search.")
(autoload 'org-notmuch-search-open "ol-notmuch" "\
Follow a notmuch search link specified by PATH.

(fn PATH _)")
(with-eval-after-load 'org (org-link-set-parameters "notmuch-tree" :store #'org-notmuch-tree-store-link :follow #'org-notmuch-tree-open))
(autoload 'org-notmuch-tree-store-link "ol-notmuch" "\
Store a link to a notmuch tree.")
(autoload 'org-notmuch-tree-open "ol-notmuch" "\
Follow a notmuch tree link specified by PATH.

(fn PATH _)")
(register-definition-prefixes "ol-notmuch" '("org-notmuch-"))

;;; End of scraped data

(provide 'ol-notmuch-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; ol-notmuch-autoloads.el ends here