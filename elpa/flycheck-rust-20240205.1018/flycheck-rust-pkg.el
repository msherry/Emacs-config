;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "flycheck-rust" "20240205.1018"
  "Flycheck: Rust additions and Cargo support."
  '((emacs     "24.1")
    (flycheck  "28")
    (dash      "2.13.0")
    (seq       "2.3")
    (let-alist "1.0.4"))
  :url "https://github.com/flycheck/flycheck-rust"
  :commit "4d365ed1c9e8b8ac43561eb365d37ab555a6e617"
  :revdesc "4d365ed1c9e8"
  :keywords '("tools" "convenience")
  :authors '(("Sebastian Wiesner" . "swiesner@lunaryorn.com"))
  :maintainers '(("Sebastian Wiesner" . "swiesner@lunaryorn.com")))
