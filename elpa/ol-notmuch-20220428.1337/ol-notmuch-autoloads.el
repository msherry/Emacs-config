;;; ol-notmuch-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ol-notmuch" "ol-notmuch.el" (0 0 0 0))
;;; Generated autoloads from ol-notmuch.el

(with-eval-after-load 'org (org-link-set-parameters "notmuch" :store #'org-notmuch-store-link :follow #'org-notmuch-open))

(autoload 'org-notmuch-store-link "ol-notmuch" "\
Store a link to one or more notmuch messages." nil nil)

(autoload 'org-notmuch-open "ol-notmuch" "\
Follow a notmuch message link specified by PATH.

\(fn PATH _)" nil nil)

(with-eval-after-load 'org (org-link-set-parameters "notmuch-search" :store #'org-notmuch-search-store-link :follow #'org-notmuch-search-open))

(autoload 'org-notmuch-search-store-link "ol-notmuch" "\
Store a link to a notmuch search." nil nil)

(autoload 'org-notmuch-search-open "ol-notmuch" "\
Follow a notmuch search link specified by PATH.

\(fn PATH _)" nil nil)

(with-eval-after-load 'org (org-link-set-parameters "notmuch-tree" :store #'org-notmuch-tree-store-link :follow #'org-notmuch-tree-open))

(autoload 'org-notmuch-tree-store-link "ol-notmuch" "\
Store a link to a notmuch tree." nil nil)

(autoload 'org-notmuch-tree-open "ol-notmuch" "\
Follow a notmuch tree link specified by PATH.

\(fn PATH _)" nil nil)

(register-definition-prefixes "ol-notmuch" '("org-notmuch-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ol-notmuch-autoloads.el ends here
