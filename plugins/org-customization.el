;;; Setup and customizations for org-mode

;;; From comments on https://emacs.stackexchange.com/questions/12475/
(defun msherry/org-save-all-org-buffers ()
  "Save all Org-mode buffers without user confirmation."
  (interactive)
  (message "Saving all Org-mode buffers...")
  (save-some-buffers t (lambda () (and (derived-mode-p 'org-mode)
                                  (buffer-file-name))))
  (when (featurep 'org-id) (org-id-locations-save))
  (message "Saving all Org-mode buffers... done"))


;;; Auto-save all org-mode buffers while agenda open -
;;; http://emacs.stackexchange.com/a/483/7169
(add-hook 'org-agenda-mode-hook
          '(lambda ()
            (add-hook 'auto-save-hook 'msherry/org-save-all-org-buffers nil t)
            (auto-save-mode t)
            ;; Muscle memory from VC mode means I hit this all the time
            (local-unset-key (kbd "x"))))

(add-hook 'org-mode-hook
          '(lambda ()
            (add-hook 'auto-save-hook 'msherry/org-save-all-org-buffers nil t)
            (auto-save-mode t)
            (auto-revert-mode)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)


;;; Persist clock history across emacs runs -
;;; http://orgmode.org/manual/Clocking-work-time.html
(setq org-clock-persist t)
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
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODO's (including scheduled) / unfiled"
         ((agenda "")
          (alltodo "")
          (tags "REFILE"
                ((org-agenda-overriding-header "To refile")))))
        ("c" "Agenda and all unscheduled TODO's / unfiled"
         ((agenda "")
          (todo ""
                ((org-agenda-overriding-header "Unscheduled TODOs")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline 'scheduled))))
          (tags "REFILE"
                ((org-agenda-overriding-header "To refile")))))
        ("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))))

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/.emacs.d/org/refile.org")
                   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/.emacs.d/org/refile.org")
                   "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/.emacs.d/org/refile.org")
                   "* %? :MEETING:\n%U" ))))

;;; Agenda file setup -- skip if we can't find the agenda directory
(if (file-accessible-directory-p "~/.emacs.d/org")
    (progn
      ;; This should be a symlink to a Dropbox dir in order to share across machines
      (setq org-agenda-files (list "~/.emacs.d/org"))
      (setq org-default-notes-file "~/.emacs.d/org/refile.org")
      )
    ;; No org/ directory, avoid setup
    (message "You seem to be missing an org/ directory in your .emacs.d -- please check for this to enable org-mode agenda tools."))

(org-clock-persistence-insinuate)


(provide 'org-customization)
