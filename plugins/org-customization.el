;;; Setup and customizations for org-mode

(require 'org-agenda-property)          ; Show properties like :LOCATION: in agenda
(require 'org-notmuch)                  ; Save links to emails

;;; From comments on https://emacs.stackexchange.com/questions/12475/
(defun msherry/org-save-all-org-buffers ()
  "Save all Org-mode buffers without user confirmation."
  (interactive)
  (message "Saving all Org-mode buffers...")
  (save-some-buffers t (lambda () (and (derived-mode-p 'org-mode)
                                  (buffer-file-name))))
  (when (featurep 'org-id) (org-id-locations-save))
  (message "Saving all Org-mode buffers... done"))


;;; Automatically update org-mode agenda
;;; http://orgmode.org/worg/org-hacks.html
(defun msherry/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in non-agenda buffers"
  ;; TODO: doesn't work unless agenda is being displayed
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    ;; TODO: would be nice to auto-update appt timers even when we don't have
    ;; an agenda buffer open
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
(run-at-time nil 300 'msherry/org-agenda-redo-in-other-window)


;;; Display popup alerts From
;;; http://orgmode.org/worg/org-hacks.html#org-agenda-appt-zenity /
;;; http://orgmode.org/worg/org-faq.html#automatic-reminders
(defun msherry/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (let ((org-deadline-warning-days 0))
    (org-agenda-to-appt)))

;; Run once, activate and schedule refresh
;;; http://doc.norang.ca/org-mode.html
(msherry/org-agenda-to-appt)
(appt-activate t)
(run-at-time "24:01" nil 'msherry/org-agenda-to-appt)

;; 10-minute warnings
(setq appt-message-warning-time 15)
(setq appt-display-interval 5)

;; Update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook #'msherry/org-agenda-to-appt)

;; Setup alerts -- tell appt to use window, and replace default function
(setq appt-display-format 'window)
(setq appt-disp-window-function #'msherry/appt-disp-window)
(setq appt-delete-window-function #'(lambda ()))  ; Popups are external to emacs, no delete needed

;; Limit inline image width
(setq org-image-actual-width '(300))

;; Killswitch for org-agenda notifications
(setq msherry-org-display-notifications t)

(defun msherry/appt-disp-window (min-to-app new-time msg)
  (if msherry-org-display-notifications
      (save-window-excursion
        (shell-command
         (concat
          "terminal-notifier"
          " -title 'Appointment'"
          " -message " (shell-quote-argument msg)
          " -sound Bottle"
          " -execute \"/usr/local/bin/emacsclient --eval '(org-agenda nil \\\"c\\\")'\"")
         nil nil))))

;;; Auto-save all org-mode buffers while agenda open -
;;; http://emacs.stackexchange.com/a/483/7169
(add-hook 'org-agenda-mode-hook
          #'(lambda ()
              ;; (add-hook 'auto-save-hook 'msherry/org-save-all-org-buffers nil t)
              ;; (auto-save-mode t)
              ;; Muscle memory from VC mode means I hit this all the time
              (local-unset-key (kbd "x"))))

(add-hook 'org-mode-hook
          #'(lambda ()
              (add-hook 'auto-save-hook 'msherry/org-save-all-org-buffers nil t)
              (auto-save-mode t)
              (auto-revert-mode)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)


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
      '(("c" "Agenda and all unscheduled/everyday TODO's / unfiled"
         ((agenda "")
          (tags "EVERYDAY"
                ((org-agenda-overriding-header "Every day")
                 (org-agenda-skip-function
                  ;; Skip the top-level headline itself
                  '(org-agenda-skip-entry-if 'regexp "Everyday"))))
          ;; All items that are not EVERYDAY items (which have their own section)
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
      '(("t" "TODO" entry (file "~/.emacs.d/org/refile.org")
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("w" "work TODO" entry (file+headline "~/.emacs.d/org/work.org" "Tasks")
         "** TODO %?\n%a\n" :clock-in t :clock-resume t)
        ("p" "personal TODO" entry (file+headline "~/.emacs.d/org/personal.org" "Tasks")
         "** TODO %?\n%a\n" :clock-in t :clock-resume t)
        ("n" "note" entry (file "~/.emacs.d/org/refile.org")
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file "~/.emacs.d/org/refile.org")
         "* %? :MEETING:\n%U" :clock-in t :clock-resume t)))

;;; Agenda file setup -- skip if we can't find the agenda directory
(if (file-accessible-directory-p "~/.emacs.d/org")
    (progn
      ;; This should be a symlink to a Dropbox dir in order to share across machines
      (setq org-agenda-files (list "~/.emacs.d/org"))
      (setq org-default-notes-file "~/.emacs.d/org/refile.org"))
    ;; No org/ directory, avoid setup
    (message "You seem to be missing an org/ directory in your .emacs.d -- please check for this to enable org-mode agenda tools."))

;;; Auto-reload generated images in org-mode when re-executing a babel code block
;;; http://emacs.stackexchange.com/a/9813/7169
(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

(defun msherry-org-agenda-get-property (property)
  "Get a property for the current headline."
  (org-agenda-check-no-diary)
  (org-entry-get (org-get-at-bol 'org-marker) property 'selective))

(defun msherry-current-agenda-unconfirmed ()
  (string= "NEEDS_ACTION" (msherry-org-agenda-get-property "ATTENDING")))

(defun color-agenda-events ()
  "Color agenda events based on what calendar they're from and other properties.

http://stackoverflow.com/a/17067170/52550"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "calendar:" nil t) ; Personal calendar
      (save-excursion
        ;; Only color agenda items, not unscheduled/to refile items
        (when (not (re-search-backward "======" nil t))
          (add-text-properties (match-beginning 0) (point-at-eol)
                               '(face font-lock-constant-face)))))
    (goto-char (point-min))
    (while (re-search-forward "pinterest:" nil t)
      (save-excursion
        ;; Color unaccepted meetings
        (when (not (re-search-backward "======" nil t))
          ;; Have to save these before we check the agenda properties, apparently
          (let ((begin (match-beginning 0))
                (end (point-at-eol)))
            (when (msherry-current-agenda-unconfirmed)
              (add-text-properties begin end '(face hi-pink)))))))))

(add-hook 'org-agenda-finalize-hook #'color-agenda-events)

(org-clock-persistence-insinuate)

;;; MobileOrg - https://mobileorg.github.io/
; These need to be set before org-mobile loads
(setq org-directory "~/.emacs.d/org")
(setq org-mobile-inbox-for-pull "~/.emacs.d/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(require 'org-mobile)
(org-mobile-pull)

;; Auto-push to MobileOrg on file saves, w/delay
;; https://github.com/matburt/mobileorg-android/wiki/FAQ
(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' uses to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(add-hook 'after-save-hook
          #'(lambda ()
              (when (eq major-mode 'org-mode)
                (dolist (file (org-mobile-files-alist))
                  (if (string= (file-truename (expand-file-name (car file)))
                               (file-truename (buffer-file-name)))
                      (org-mobile-push-with-delay 30))))))

;; Auto-pull changes on Dropbox change notifications
(defun install-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (unless (< p (float-time (time-since (elt (file-attributes f) 5))))
       (org-mobile-pull)))
   file secs))

(install-monitor (file-truename
                  (concat
                   (file-name-as-directory org-mobile-directory)
                   org-mobile-capture-file))
                 60)

;; Do a pull every 5 minutes to circumvent problems with timestamping
;; (ie. dropbox bugs)
(run-with-timer 0 (* 5 60) 'org-mobile-pull)


;;; Org-contrib stuff
(require 'ox-extra)

;; Enable :ignore: tags to ignore headlines while keeping their content in exports
;; http://emacs.stackexchange.com/a/17677/7169
(ox-extras-activate '(ignore-headlines))


(provide 'org-customization)
