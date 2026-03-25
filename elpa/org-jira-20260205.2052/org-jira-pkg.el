;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-jira" "20260205.2052"
  "Syncing between Jira and Org-mode."
  '((emacs   "24.5")
    (cl-lib  "0.5")
    (request "0.2.0")
    (dash    "2.14.1"))
  :url "https://github.com/ahungry/org-jira"
  :commit "738002ddbeb0b4311089706f0a979ceea53bf095"
  :revdesc "738002ddbeb0"
  :keywords '("ahungry" "jira" "org" "bug" "tracker")
  :maintainers '(("Matthew Carter" . "m@ahungry.com")))
