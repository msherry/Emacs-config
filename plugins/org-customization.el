;;; Setup and customizations for org-mode

(require 'org)
(require 'org-agenda-property)          ; Show properties like :LOCATION: in agenda
(require 'org-id)                       ; global identifiers for org entries
(require 'org-super-agenda)

;;; Commentary:

;;; Code:

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
  "Call org-agenda-redo function even in non-agenda buffers."
  ;; TODO: doesn't work unless agenda is being displayed
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    ;; TODO: would be nice to auto-update appt timers even when we don't have
    ;; an agenda buffer open
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
;(run-at-time nil 300 'msherry/org-agenda-redo-in-other-window)


;;; Display popup alerts From
;;; http://orgmode.org/worg/org-hacks.html#org-agenda-appt-zenity /
;;; http://orgmode.org/worg/org-faq.html#automatic-reminders
(defun msherry/org-agenda-to-appt ()
  (setq appt-time-msg-list nil)
  (let ((org-deadline-warning-days 0))
    (org-agenda-to-appt)))

;; Run once, activate and schedule refresh
;;; http://doc.norang.ca/org-mode.html
(msherry/org-agenda-to-appt)
(appt-activate t)
(run-at-time "24:01" nil 'msherry/org-agenda-to-appt)

;; Update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook #'msherry/org-agenda-to-appt)

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
          ; terminal-notifier doesn't seem to exit now when expecting
          ; input. https://github.com/julienXX/terminal-notifier/issues/173 is
          ; one possible issue that claims to solve it, but it still happens.
          ;; " -execute \"/usr/local/bin/emacsclient --eval '(org-agenda nil \"c\")'\""
          "&"                           ; async
          )
         nil nil))))

(defun org-show-current-heading-tidily ()
  "Show current entry, keeping other entries closed."
  ;; https://stackoverflow.com/a/28031539/52550
  (interactive)
  (if (save-excursion
        (end-of-line)
        (outline-invisible-p))
      (progn
        (org-show-entry)
        (outline-show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-at-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

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
              (auto-revert-mode)
              (visual-line-mode)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)


;; http://doc.norang.ca/org-mode.html
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion.  this is obsolete in org 9.0+,
; use ido-ubiquitous-mode instead and set ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-max-directory-size 100000)
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)


;;; org-super-agenda (https://github.com/alphapapa/org-super-agenda) is pretty
;;; cool
(setq org-super-agenda-groups
      nil)
      ;; '((:log t)  ; Automatically named "Log"
      ;;   (:name "Schedule"
      ;;    :time-grid t)
      ;;   (:name "Today"
      ;;    :scheduled today)
      ;;   (:habit t)
      ;;   (:name "Overdue"
      ;;    :deadline past)
      ;;   (:name "Due today"
      ;;    :deadline today)
      ;;   (:name "Due soon"
      ;;    :deadline future)
      ;;   (:name "Unimportant"
      ;;    :todo ("SOMEDAY" "MAYBE" "CHECK" "TO_READ" "TO_WATCH")
      ;;    :order 100)
      ;;   (:name "Waiting..."
      ;;    :todo "WAITING"
      ;;    :order 98)
      ;;   (:name "Scheduled earlier"
      ;;    :scheduled past)
      ;;   (:name "Every day"
      ;;    :tag "EVERYDAY"
      ;;    :order 1)))

(org-super-agenda-mode 1)

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
  "Force redisplay of inline images, if present."
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

(defun msherry-org-agenda-get-property (property)
  "Get the value of a property, named via PROPERTY, for the current headline."
  (org-agenda-check-no-diary)
  (org-entry-get (org-get-at-bol 'org-marker) property 'selective))

(defun msherry-current-agenda-unconfirmed ()
  "Return t if attendance for the current agenda item is still unconfirmed."
  (member (msherry-org-agenda-get-property "ATTENDING") '("NEEDS_ACTION" "UNSET")))

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
    (while (re-search-forward "futureproof:" nil t)
      (save-excursion
        ;; Color unaccepted meetings
        (when (not (re-search-backward "======" nil t))
          ;; Have to save these before we check the agenda properties, apparently
          (let ((begin (match-beginning 0))
                (end (point-at-eol)))
            (when (msherry-current-agenda-unconfirmed)
              (add-text-properties begin end '(face hi-pink)))))))))
(add-hook 'org-agenda-finalize-hook #'color-agenda-events)

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
         (* 1 secs) nil #'(lambda () (unless (buffer-modified-p) (org-mobile-push))))))

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


;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;;; Clocking in/out -- from http://doc.norang.ca/org-mode.html and
;;; http://aaron.baugher.biz/emacs/org-mode/back-to-last-task (for resuming
;;; interrupted tasks)

(setq bh/keep-clock-running nil)
(defvar bh/default-task-id "FD7773C6-7F61-4B1F-B2D0-B82D64153E2C")
(defvar bh/mail-task-id "F169512E-FDD6-4429-A290-9B566A40301D")


(defun msherry/get-email-task-name ()
  (org-with-point-at (org-id-find bh/mail-task-id 'marker)
    (nth 4 (org-heading-components))))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Wasting time task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun msherry/clock-in-email-task (&rest args)
  (save-excursion
    (org-with-point-at (org-id-find bh/mail-task-id 'marker)
      (org-clock-in))))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/default-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defun msherry/clock-in-default-task ()
  "Clock in to the default task. Based on bh/clock-in-parent-task"
  (when bh/keep-clock-running
    (bh/clock-in-default-task)))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (if (marker-buffer org-clock-interrupted-task)
        (org-with-point-at org-clock-interrupted-task
          (org-clock-in))
      (msherry/clock-in-default-task))))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
(add-hook 'org-clock-out-hook 'org-save-all-org-buffers 'append)

;; Automatically clock in/out of persistent email task when reading/closing
;; email
(defun msherry/clock-out-of-email (&rest args)
  "If we're not in a notmuch buffer (search or show), and we're current clocked into the email task, clock out."
  (unless (derived-mode-p 'notmuch-search-mode 'notmuch-show-mode)
    (when (and (org-clock-is-active)
               (equal org-clock-current-task (msherry/get-email-task-name)))
      (org-clock-out))))

;; (advice-add 'msherry-notmuch-unread :before #'msherry/clock-in-email-task)
;; (advice-add 'notmuch-bury-or-kill-this-buffer :after #'msherry/clock-out-of-email)

;;; Flyspell-mode clobbers this
(define-key org-mode-map (kbd "s-<tab>") #'pcomplete)

;;; org-mru-clock stuff
;; (global-set-key (kbd "C-c C-x i") #'org-mru-clock-in)
;; (global-set-key (kbd "C-c C-x C-j") #'org-mru-clock-select-recent-task)


;;; From https://orgmode.org/worg/org-tutorials/org-meeting-tasks.html
(defcustom org-mactions-numbered-action-format "TODO Action #%d "
  "Default structure of the headling of a new action.
    %d will become the number of the action."
  :group 'org-edit-structure
  :type 'string)

(defcustom org-mactions-change-id-on-copy t
  "Non-nil means make new IDs in copied actions.
If an action copied with the command `org-mactions-collect-todos-in-subtree'
contains an ID, that ID will be replaced with a new one."
  :group 'org-edit-structure
  :type 'string)

(defun org-mactions-new-numbered-action (&optional inline)
  "Insert a new numbered action, using `org-mactions-numbered-action-format'.
    With prefix argument, insert an inline task."
  (interactive "P")
  (let* ((num (let ((re "\\`#\\([0-9]+\\)\\'"))
                (1+ (apply 'max 0
                           (mapcar
                            (lambda (e)
                              (if (string-match re (car e))
                                  (string-to-number (match-string 1 (car e)))
                                0))
                            (org-get-buffer-tags))))))
         (tag (concat "#" (number-to-string num))))
    (if inline
        (org-inlinetask-insert-task)
      (org-insert-heading 'force))
    (unless (eql (char-before) ?\ ) (insert " "))
    (insert (format org-mactions-numbered-action-format num))
    (org-toggle-tag tag 'on)
    (if (= (point-max) (point-at-bol))
        (save-excursion (goto-char (point-at-eol)) (insert "\n")))
    (unless (eql (char-before) ?\ ) (insert " "))))

(defun org-mactions-collect-todos-in-subtree ()
  "Collect all TODO items in the current subtree into a flat list."
  (interactive)
  (let ((buf (get-buffer-create "Org TODO Collect"))
        (cnt 0) beg end string s)
    (with-current-buffer buf (erase-buffer) (org-mode))
    (org-map-entries
     (lambda ()
       (setq beg (point) end (org-end-of-subtree t t) cnt (1+ cnt)
             string (buffer-substring beg end)
             s 0)
       (when org-mactions-change-id-on-copy
         (while (string-match "^\\([ \t]*:ID:\\)[ \t\n]+\\([^ \t\n]+\\)[ \t]*$"
                              string s)
           (setq s (match-end 1)
                 string (replace-match (concat "\\1 "
                                               (save-match-data (org-id-new)))
                                       t nil string))))
       (with-current-buffer buf (org-paste-subtree 1 string)
                            (goto-char (point-max))))
     (format "TODO={%s}" (regexp-opt org-not-done-keywords))
     'tree)
    (if (= cnt 0)
        (message "No TODO items in subtree")
      (message "%d TODO entries copied to kill ring" cnt)
      (prog1 (with-current-buffer buf
               (kill-new (buffer-string)))
        (kill-buffer buf)))))

(defun export-filter-local-to-mirth (backend)
  "Convert local file links to shareable links before exporting org files when `BACKEND' is supported."
  (when (memq backend '(html md))
    (save-window-excursion
      (beginning-of-buffer)
      (while (re-search-forward "\\[\\[\\(file:~/src/all-the-things.*?\\)\\]\\[" nil t)
        (replace-match
         (save-excursion
           (save-match-data
             (msherry/org-open-at-point-no-buffer-shenanigans t)
             (mirth-find-url)))
           nil nil nil 1)
        )
      )))

(add-hook 'org-export-before-parsing-hook #'export-filter-local-to-mirth)

;; (add-to-list 'org-export-filter-paragraph-functions 'export-filter-local-to-mirth)

(defun msherry/org-open-at-point-no-buffer-shenanigans (&optional arg reference-buffer)
  "Exactly like org-open-at-point except that (buffer-file-name)
works as I expect it to when calling this programmatically."
  (interactive "P")
  ;; On a code block, open block's results.
  (unless (call-interactively 'org-babel-open-src-block-result)
    (org-load-modules-maybe)
    (setq org-window-config-before-follow-link (current-window-configuration))
    (org-remove-occur-highlights nil nil t)
    (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
      (let* ((context
	      ;; Only consider supported types, even if they are not
	      ;; the closest one.
	      (org-element-lineage
	       (org-element-context)
	       '(clock footnote-definition footnote-reference headline
		       inlinetask link timestamp)
	       t))
	     (type (org-element-type context))
	     (value (org-element-property :value context)))
	(cond
	 ;; On a headline or an inlinetask, but not on a timestamp,
	 ;; a link, a footnote reference.
	 ((memq type '(headline inlinetask))
	  (org-match-line org-complex-heading-regexp)
	  (if (and (match-beginning 5)
		   (>= (point) (match-beginning 5))
		   (< (point) (match-end 5)))
	      ;; On tags.
	      (org-tags-view arg (substring (match-string 5) 0 -1))
	    ;; Not on tags.
	    (pcase (org-offer-links-in-entry (current-buffer) (point) arg)
	      (`(nil . ,_)
	       (require 'org-attach)
	       (org-attach-reveal 'if-exists))
	      (`(,links . ,links-end)
	       (dolist (link (if (stringp links) (list links) links))
		 (search-forward link nil links-end)
		 (goto-char (match-beginning 0))
		 (org-open-at-point))))))
	 ;; On a footnote reference or at definition's label.
	 ((or (eq type 'footnote-reference)
	      (and (eq type 'footnote-definition)
		   (save-excursion
		     ;; Do not validate action when point is on the
		     ;; spaces right after the footnote label, in
		     ;; order to be on par with behavior on links.
		     (skip-chars-forward " \t")
		     (let ((begin
			    (org-element-property :contents-begin context)))
		       (if begin (< (point) begin)
			 (= (org-element-property :post-affiliated context)
			    (line-beginning-position)))))))
	  (org-footnote-action))
	 ;; No valid context.  Ignore catch-all types like `headline'.
	 ;; If point is on something looking like a link or
	 ;; a time-stamp, try opening it.  It may be useful in
	 ;; comments, example blocks...
	 ((memq type '(footnote-definition headline inlinetask nil))
	  (call-interactively #'org-open-at-point-global))
	 ;; On a clock line, make sure point is on the timestamp
	 ;; before opening it.
	 ((and (eq type 'clock)
	       value
	       (>= (point) (org-element-property :begin value))
	       (<= (point) (org-element-property :end value)))
	  (org-follow-timestamp-link))
	 ;; Do nothing on white spaces after an object.
	 ((>= (point)
	      (save-excursion
		(goto-char (org-element-property :end context))
		(skip-chars-backward " \t")
		(point)))
	  (user-error "No link found"))
	 ((eq type 'timestamp) (org-follow-timestamp-link))
	 ((eq type 'link)
	  (let ((type (org-element-property :type context))
		(path (org-link-unescape (org-element-property :path context))))
	    ;; Switch back to REFERENCE-BUFFER needed when called in
	    ;; a temporary buffer through `org-open-link-from-string'.
	    (cond
	      ((equal type "file")
	       (if (string-match "[*?{]" (file-name-nondirectory path))
		   (dired path)
		 ;; Look into `org-link-parameters' in order to find
		 ;; a DEDICATED-FUNCTION to open file.  The function
		 ;; will be applied on raw link instead of parsed
		 ;; link due to the limitation in `org-add-link-type'
		 ;; ("open" function called with a single argument).
		 ;; If no such function is found, fallback to
		 ;; `org-open-file'.
		 (let* ((option (org-element-property :search-option context))
			(app (org-element-property :application context))
			(dedicated-function
			 (org-link-get-parameter
			  (if app (concat type "+" app) type)
			  :follow)))
		   (if dedicated-function
		       (funcall dedicated-function
				(concat path
					(and option (concat "::" option))))
		     (apply #'org-open-file
			    path
			    (cond (arg)
				  ((equal app "emacs") 'emacs)
				  ((equal app "sys") 'system))
			    (cond ((not option) nil)
				  ((string-match-p "\\`[0-9]+\\'" option)
				   (list (string-to-number option)))
				  (t (list nil
					   (org-link-unescape option)))))))))
	      ((functionp (org-link-get-parameter type :follow))
	       (funcall (org-link-get-parameter type :follow) path))
	      ((member type '("coderef" "custom-id" "fuzzy" "radio"))
	       (unless (run-hook-with-args-until-success
			'org-open-link-functions path)
		 (if (not arg) (org-mark-ring-push)
		   (switch-to-buffer-other-window
		    (org-get-buffer-for-internal-link (current-buffer))))
		 (let ((destination
			(org-with-wide-buffer
			 (if (equal type "radio")
			     (org-search-radio-target
			      (org-element-property :path context))
			   (org-link-search
			    (if (member type '("custom-id" "coderef"))
				(org-element-property :raw-link context)
			      path)
			    ;; Prevent fuzzy links from matching
			    ;; themselves.
			    (and (equal type "fuzzy")
				 (+ 2 (org-element-property :begin context)))))
			 (point))))
		   (unless (and (<= (point-min) destination)
				(>= (point-max) destination))
		     (widen))
		   (goto-char destination))))
	      (t (browse-url-at-point)))))
	 (t (user-error "No link found")))))
    (run-hook-with-args 'org-follow-link-hook)))

(defun msherry/org-babel-result-hide-all ()
  "Fold all results in the current buffer.

  Fixes a bug in the default version in that only results after point are hidden."
  (interactive)
  (org-babel-show-result-all)
  (save-excursion
    (goto-char (point-min))             ; the real version should do this
    (let ((case-fold-search t))
      (while (re-search-forward org-babel-result-regexp nil t)
	(save-excursion (goto-char (match-beginning 0))
			(org-babel-hide-result-toggle-maybe))))))


;; https://emacs.stackexchange.com/a/33391/7169
(defun org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
          ((looking-at-p block-re)
           (when (< content-line-count 2)
             (delete-region start-pos (1+ (point-at-bol))))
           (setq start-pos (point))
           (forward-line)
           (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
          ((not (looking-at-p blank-line-re))
           (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))

(add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)


(provide 'org-customization)
;;; org-customization.el ends here
