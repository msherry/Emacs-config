;; Stolen from a Linux system, obviously. Depends on edict dictionaries being
;; installed in /usr/local/edict/edict, as a Debian-based system would.

;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux edict-el package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

(autoload 'edict-search-english "edict" "Search an English word" t)
(autoload 'edict-search-kanji "edict" "Search for a Japanese word or compound" t)
(setq edict-dictionaries
      '(("/usr/share/edict/edict" . euc-jp)))

(provide 'load-edict)
