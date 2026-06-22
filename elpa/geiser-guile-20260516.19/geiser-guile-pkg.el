;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "geiser-guile" "20260516.19"
  "Guile and Geiser talk to each other."
  '((emacs     "27.1")
    (transient "0.3")
    (geiser    "0.33"))
  :url "https://gitlab.com/emacs-geiser/guile"
  :commit "cbab81bd2dcb4c787bcda4ae18062db3087e6887"
  :revdesc "cbab81bd2dcb"
  :keywords '("languages" "guile" "scheme" "geiser")
  :authors '(("Jose Antonio Ortega Ruiz" . "(jao@gnu.org)"))
  :maintainers '(("Jose Antonio Ortega Ruiz" . "(jao@gnu.org)")))
