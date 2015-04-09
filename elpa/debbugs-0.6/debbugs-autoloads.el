;;; debbugs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (debbugs-gnu-bugs debbugs-gnu-usertags debbugs-gnu
;;;;;;  debbugs-gnu-search) "debbugs-gnu" "debbugs-gnu.el" (21604
;;;;;;  60163 0 0))
;;; Generated autoloads from debbugs-gnu.el

(autoload 'debbugs-gnu-search "debbugs-gnu" "\
Search for Emacs bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\".

\(fn)" t nil)

(autoload 'debbugs-gnu "debbugs-gnu" "\
List all outstanding bugs.

\(fn SEVERITIES &optional PACKAGES ARCHIVEDP SUPPRESS TAGS)" t nil)

(autoload 'debbugs-gnu-usertags "debbugs-gnu" "\
List all user tags for USERS, which is (\"emacs\") by default.

\(fn &rest USERS)" t nil)

(autoload 'debbugs-gnu-bugs "debbugs-gnu" "\
List all BUGS, a list of bug numbers.

\(fn &rest BUGS)" t nil)

;;;***

;;;### (autoloads (debbugs-org-bugs debbugs-org-mode debbugs-org
;;;;;;  debbugs-org-search) "debbugs-org" "debbugs-org.el" (21604
;;;;;;  60162 0 0))
;;; Generated autoloads from debbugs-org.el

(autoload 'debbugs-org-search "debbugs-org" "\
Search for bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.

\(fn)" t nil)

(autoload 'debbugs-org "debbugs-org" "\
List all outstanding bugs.

\(fn SEVERITIES &optional PACKAGES ARCHIVEDP SUPPRESS TAGS)" t nil)

(autoload 'debbugs-org-mode "debbugs-org" "\
Minor mode for providing a debbugs interface in org-mode buffers.

\\{debbugs-org-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'debbugs-org-bugs "debbugs-org" "\
List all BUGS, a list of bug numbers.

\(fn &rest BUGS)" t nil)

;;;***

;;;### (autoloads nil nil ("debbugs-pkg.el" "debbugs.el") (21604
;;;;;;  60163 220591 0))

;;;***

(provide 'debbugs-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; debbugs-autoloads.el ends here
