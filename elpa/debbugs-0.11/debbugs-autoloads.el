;;; debbugs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "debbugs" "debbugs.el" (0 0 0 0))
;;; Generated autoloads from debbugs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "debbugs" '("debbugs-")))

;;;***

;;;### (autoloads nil "debbugs-browse" "debbugs-browse.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from debbugs-browse.el

(autoload 'debbugs-browse-mode "debbugs-browse" "\
Browse GNU Debbugs bug URLs with debbugs-gnu or debbugs-org.
With a prefix argument ARG, enable Debbugs Browse mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.
The customer option `debbugs-browse-function' controls, which of
the two packages is used for showing bugs.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "debbugs-browse" '("debbugs-browse-")))

;;;***

;;;### (autoloads nil "debbugs-gnu" "debbugs-gnu.el" (0 0 0 0))
;;; Generated autoloads from debbugs-gnu.el

(autoload 'debbugs-gnu-search "debbugs-gnu" "\
Search for Emacs bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\".

\(fn)" t nil)

(autoload 'debbugs-gnu-patches "debbugs-gnu" "\
List the bug reports that have been marked as containing a patch.

\(fn)" t nil)

(autoload 'debbugs-gnu "debbugs-gnu" "\
List all outstanding bugs.

\(fn SEVERITIES &optional PACKAGES ARCHIVEDP SUPPRESS TAGS)" t nil)

(autoload 'debbugs-gnu-usertags "debbugs-gnu" "\
List all user tags for USERS, which is (\"emacs\") by default.

\(fn &rest USERS)" t nil)

(autoload 'debbugs-gnu-bugs "debbugs-gnu" "\
List all BUGS, a list of bug numbers.
In interactive calls, prompt for a comma separated list of bugs
or bug ranges, with default to `debbugs-gnu-default-bug-number-list'.

\(fn &rest BUGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "debbugs-gnu" '("debbugs-")))

;;;***

;;;### (autoloads nil "debbugs-org" "debbugs-org.el" (0 0 0 0))
;;; Generated autoloads from debbugs-org.el

(autoload 'debbugs-org-search "debbugs-org" "\
Search for bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\".

\(fn)" t nil)

(autoload 'debbugs-org-patches "debbugs-org" "\
List the bug reports that have been marked as containing a patch.

\(fn)" t nil)

(autoload 'debbugs-org "debbugs-org" "\
List all outstanding bugs.

\(fn)" t nil)

(autoload 'debbugs-org-mode "debbugs-org" "\
Minor mode for providing a debbugs interface in org-mode buffers.

\\{debbugs-org-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'debbugs-org-bugs "debbugs-org" "\
List all BUGS, a list of bug numbers.
In interactive calls, prompt for a comma separated list of bugs
or bug ranges, with default to `debbugs-gnu-default-bug-number-list'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "debbugs-org" '("debbugs-org-")))

;;;***

;;;### (autoloads nil nil ("debbugs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; debbugs-autoloads.el ends here
