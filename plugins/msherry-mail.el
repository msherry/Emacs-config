;;; Mail sending
(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;; Browsing
(require 'cl)
(require 'notmuch)
(require 'url)
(require 's)

(setq mm-text-html-renderer 'w3m)
(setq notmuch-fcc-dirs nil)             ; Gmail saves sent mails by itself

(defvar msherry-email-update-file-path "/tmp/offlineimap_sync_required"
  "Touch this file to force the external offlineimap-runner.sh to resync.")

(defun msherry-notmuch-unread (arg)
  "Jump immediately to unread emails in notmuch.

With a prefix argument, jump to the `notmuch' home screen."
  (interactive "P")
  ;; TODO: use the saved 'u' search here
  (if arg (notmuch)
    (notmuch-search "tag:unread AND tag:INBOX" (default-value 'notmuch-search-oldest-first))))

(global-set-key (kbd "C-c m") #'msherry-notmuch-unread)

;;; Tagging keybindings

(defun msherry--toggle-tag-search-or-show (tag-name)
  "Toggle the `tag-name' flag in notmuch search or show modes"
  (let* (; are we in search or show mode?
         (current-mode (cond ((derived-mode-p 'notmuch-search-mode) 'search)
                             ((derived-mode-p 'notmuch-show-mode) 'show)))
         ; search and show mode have different ways to tag
         (tag-fn (ecase current-mode
                   ('search #'notmuch-search-tag)
                   ('show #'notmuch-show-tag-message)))
         ; search and show mode have different ways to query tags
         (tag-query-fn (ecase current-mode
                         ('search #'notmuch-search-get-tags)
                         ('show #'notmuch-show-get-tags)))
         ; is the tag currently present?
         (tag-present (member tag-name (funcall tag-query-fn)))
         ; show and search mode tagging functions take args raw or in a list,
         ; respectively
         (tag-changes (funcall (ecase current-mode
                                 ('search #'list)
                                 ('show #'identity))
                               (concat (if tag-present "-" "+") tag-name))))
    (funcall tag-fn tag-changes)))

(defun msherry-toggle-unread ()
    (interactive)
    (msherry--toggle-tag-search-or-show "unread"))
(defun msherry-toggle-flagged ()
    (interactive)
    (msherry--toggle-tag-search-or-show "flagged"))
(defun msherry-toggle-muted ()
    (interactive)
    (msherry--toggle-tag-search-or-show "muted"))

;; Open links in emails
(define-key notmuch-show-mode-map (kbd  "C-c C-o")
  (lambda ()
    "Open link at point"
    (interactive)
    (browse-url (url-get-url-at-point))))

;; Search (list) mode keybindings

; Archive mail
(define-key notmuch-search-mode-map "y"
  ;;; TODO: replace this with call to notmuch-show-archive-thread-then-next
      (lambda (&optional beg end)
        "Remove thread from inbox"
        (interactive (notmuch-search-interactive-region))
        (notmuch-search-tag (list "-INBOX") beg end)
        (notmuch-refresh-this-buffer)))

; Mute mail in search mode
(define-key notmuch-search-mode-map "M"
  (lambda ()
    "Mute the current thread"
    (interactive)
    (msherry-toggle-muted)))

; Toggle unread
(define-key notmuch-search-mode-map "u" #'msherry-toggle-unread)

; Toggle flagged (Gmail starred)
(define-key notmuch-search-mode-map "S" #'msherry-toggle-flagged)

;; Show mode keybindings

; Archive mail in show mode
(define-key notmuch-show-mode-map "y"
  ;;; TODO: replace this with call to notmuch-show-archive-thread-then-next
  (lambda ()
    "Remove all shown messages in thread from inbox, return to search mode"
    (interactive)
    (notmuch-show-tag-all (list "-INBOX"))
    (notmuch-bury-or-kill-this-buffer)
    (notmuch-refresh-this-buffer)
    (notmuch-search-last-thread)))

; Mute mail in show mode
(define-key notmuch-show-mode-map "M"
  (lambda ()
    "Mute the current thread"
    (interactive)
    (msherry-toggle-muted)))

; Toggle unread in show mode
(define-key notmuch-show-mode-map "u" #'msherry-toggle-unread)

; Toggle star
(define-key notmuch-show-mode-map "S" #'msherry-toggle-flagged)

;; macros for quickly toggling tags - https://notmuchmail.org/emacstips/#index6h2
(eval-after-load 'notmuch-show
  '(define-key notmuch-show-mode-map "`" 'notmuch-show-apply-tag-macro))

(setq notmuch-show-tag-macro-alist
      (list
       '("m" "+Muted")
       '("u" "+unread")
       '("y" "-INBOX")
       ))

(defun notmuch-show-apply-tag-macro (key)
  (interactive "k")
  (let ((macro (assoc key notmuch-show-tag-macro-alist)))
    (apply 'notmuch-show-tag-message (cdr macro))))


;;; For display-time-mode
(defun msherry-new-important-mail ()
  "Return t if new important mail, else nil

https://gist.github.com/dbp/9627194"
  ;; TODO: if the current buffer is a remote file (over TRAMP), then the shell
  ;; command is executed on the remote host, and notmuch is likely not present,
  ;; causing this to mistakenly return t
  (if (string= (s-chomp
                (shell-command-to-string "/usr/local/bin/notmuch count tag:INBOX and tag:unread"))
               "0")
      nil
    t))


(defun msherry-notmuch-show-redraw-tags ()
  "Redraw all tags in the current message based on their current state"
  (notmuch-show-update-tags '())
  (notmuch-show-update-tags (notmuch-show-get-tags))
  )

;; Fix up broken functions
(defun msherry-notmuch-redisplay-search-with-highlight (&rest args)
  (interactive)
  (notmuch-refresh-this-buffer)
  (notmuch-hl-line-mode))
(advice-add 'notmuch-search-archive-thread
            :after #'msherry-notmuch-redisplay-search-with-highlight)

;;; Update mail flag more often
(advice-add 'notmuch-refresh-this-buffer :after #'display-time-update)


(add-hook 'notmuch-after-tag-hook
          #'(lambda (&rest rest)
              (shell-command (concat "touch " (shell-quote-argument msherry-email-update-file-path)))))


(defun msherry-highlight-myself (&rest args)
  "Note - this doesn't use font-lock-mode, so it's not updated on the fly"
  (dolist (regex (list "\\<\\([Mm]arc\\)\\>"))
    (unhighlight-regexp regex)          ; Unhighlight to force a full redraw
    (highlight-regexp regex 'font-lock-constant-face)))
(advice-add 'notmuch-show-insert-msg :after #'msherry-highlight-myself)
(add-hook 'notmuch-show-hook #'msherry-highlight-myself)

(defun offlineimap-get-password (user host port)
  ;; TODO: this is probably pretty insecure, maybe even worse than just leaving
  ;; a cleartext password in .offlineimaprc -- at least that would only expose
  ;; one password, not potentially every one. Lock this down.
  (let* ((secret
          (plist-get (nth 0 (auth-source-search :user user :host host :port port :max 1 :require '(:secret))) :secret))
         (real-secret (if (functionp secret)
                          (funcall secret)
                        secret)))
    real-secret))


(provide 'msherry-mail)
