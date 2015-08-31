;;; Setup and customizations for org-mode

(if (file-accessible-directory-p "~/.emacs.d/org")
    (progn
      (global-set-key (kbd "C-c a") 'org-agenda)
      (global-set-key (kbd "C-c c") 'org-capture)
                                        ; This should be a symlink to a Dropbox dir
      (setq org-agenda-files (list "~/.emacs.d/org"))
      (setq org-default-notes-file "~/.emacs.d/org/refile.org")
      (setq org-agenda-start-on-weekday 0)
      (setq org-log-done t)
      ;; http://doc.norang.ca/org-mode.html
      ;; Targets include this file and any file contributing to the agenda - up
      ;; to 9 levels deep
      (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                       (org-agenda-files :maxlevel . 9))))
      ;; Use full outline paths for refile targets - we file directly with IDO
      (setq org-refile-use-outline-path t)
      ;; Targets complete directly with IDO
      (setq org-outline-path-complete-in-steps nil)
      ;; Allow refile to create parent tasks with confirmation
      (setq org-refile-allow-creating-parent-nodes (quote confirm))
      ;; Use IDO for both buffer and file completion and ido-everywhere to t
      (setq org-completion-use-ido t)
      (setq org-agenda-time-grid '((daily today today)
                                   #("----------------" 0 16 (org-heading t))
                                   (800 1000 1200 1400 1600 1800 2000)))
      (setq org-capture-templates
            (quote (("t" "todo" entry (file "~/.emacs.d/org/refile.org")
                         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                    ("n" "note" entry (file "~/.emacs.d/org/refile.org")
                         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                    ("m" "Meeting" entry (file "~/.emacs.d/org/refile.org")
                         "* %? :MEETING:\n%U" :clock-in t :clock-resume t))))
      (setq org-agenda-custom-commands
            '(("n" "Agenda and all TODO's/unfiled" ((agenda "") (alltodo "") (tags "REFILE")))
              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))))
      )
    ;; No org/ directory, avoid setup
    (message "You seem to be missing an org/ directory in your .emacs.d -- please check for this to enable org-mode agenda tools."))

(provide 'org-customization)
