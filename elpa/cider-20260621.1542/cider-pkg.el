;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "cider" "20260621.1542"
  "Clojure Interactive Development Environment that Rocks."
  '((emacs        "28")
    (clojure-mode "5.19")
    (compat       "30")
    (parseedn     "1.2.1")
    (queue        "0.2")
    (spinner      "1.7")
    (seq          "2.22")
    (sesman       "0.3.2")
    (transient    "0.4.1"))
  :url "https://www.github.com/clojure-emacs/cider"
  :commit "79cd552aea5b4a6a4c62891c5dabf126610e397b"
  :revdesc "79cd552aea5b"
  :keywords '("languages" "clojure" "cider")
  :authors '(("Tim King" . "kingtim@gmail.com")
             ("Phil Hagelberg" . "technomancy@gmail.com")
             ("Bozhidar Batsov" . "bozhidar@batsov.dev")
             ("Artur Malabarba" . "bruce.connor.am@gmail.com")
             ("Hugo Duncan" . "hugo@hugoduncan.org")
             ("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Bozhidar Batsov" . "bozhidar@batsov.dev")))
