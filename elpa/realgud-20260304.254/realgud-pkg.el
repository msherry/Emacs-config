;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "realgud" "20260304.254"
  "A modular front-end for interacting with external debuggers."
  '((load-relative "1.3.2")
    (loc-changes   "1.2")
    (test-simple   "1.3.0")
    (emacs         "27"))
  :url "https://github.com/realgud/realgud/"
  :commit "6a3c23764353a5f94a35f212a482b12346ea03d4"
  :revdesc "6a3c23764353"
  :keywords '("debugger" "gdb" "python" "perl" "go" "bash" "zsh" "bashdb" "zshdb" "remake" "trepan" "perldb" "pdb")
  :authors '(("Rocky Bernstein" . "rocky@gnu.org"))
  :maintainers '(("Rocky Bernstein" . "rocky@gnu.org")))
