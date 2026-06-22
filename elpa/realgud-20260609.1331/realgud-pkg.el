;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "realgud" "20260609.1331"
  "A modular front-end for interacting with external debuggers."
  '((load-relative "1.3.2")
    (loc-changes   "1.2")
    (test-simple   "1.3.0")
    (emacs         "27"))
  :url "https://github.com/realgud/realgud/"
  :commit "bc3fbafc190a1a38fd12459bf323ffe4dcb159af"
  :revdesc "bc3fbafc190a"
  :keywords '("debugger" "gdb" "python" "perl" "go" "bash" "zsh" "bashdb" "zshdb" "remake" "trepan" "perldb" "pdb")
  :authors '(("Rocky Bernstein" . "rocky@gnu.org"))
  :maintainers '(("Rocky Bernstein" . "rocky@gnu.org")))
