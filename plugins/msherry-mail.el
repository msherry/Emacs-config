;;; Mail sending
(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;; Browsing
(require 'notmuch)
(require 'url)
(require 's)

(setq mm-text-html-renderer 'w3m)
(setq notmuch-fcc-dirs nil)             ; Gmail saves sent mails by itself

(defun msherry-notmuch-unread (arg)
  "Jump immediately to unread emails in notmuch.

With a prefix argument, jump to the `notmuch' home screen."
  (interactive "P")
  ;; TODO: use the saved 'u' search here
  (if arg (notmuch)
    (notmuch-search "tag:unread AND tag:INBOX")))

(global-set-key (kbd "C-c m") #'msherry-notmuch-unread)

;;; Tagging keybindings

;; Open links in emails
(define-key notmuch-show-mode-map (kbd  "C-c C-o")
  (lambda ()
    "Open link at point"
    (interactive)
    (browse-url (url-get-url-at-point))))

;; Search (list) mode keybindings

; Archive mail
(define-key notmuch-search-mode-map "y"
      (lambda (&optional beg end)
        "Remove thread from inbox"
        (interactive (notmuch-search-interactive-region))
        (notmuch-search-tag (list "-INBOX") beg end)
        (notmuch-refresh-this-buffer)))

; Toggle unread
(define-key notmuch-search-mode-map "u"
  (lambda ()
    "Toggle unread tag for message"
    (interactive)
    (notmuch-search-tag
     (if (member "unread" (notmuch-search-get-tags))
         (list "-unread") (list "+unread")))))

; Toggle star
(define-key notmuch-search-mode-map "S"
  (lambda ()
    "Toggle unread tag for message"
    (interactive)
    (notmuch-search-tag
     (if (member "[Gmail].Starred" (notmuch-search-get-tags))
         (list "-[Gmail].Starred" "-flagged") (list "+[Gmail].Starred" "+flagged")))))

;; Show mode keybindings

; Archive mail in show mode
(define-key notmuch-show-mode-map "y"
  (lambda ()
    "Remove all shown messages in thread from inbox, return to search mode"
    (interactive)
    (notmuch-show-tag-all (list "-INBOX"))
    (notmuch-bury-or-kill-this-buffer)
    ;; Move to last message - http://stackoverflow.com/a/24969047/52550
    (let ((column (current-column)))
      (goto-char (point-max))
      (forward-line -2)
      (move-to-column column))
    (notmuch-refresh-this-buffer)))

; Toggle unread in show mode
(define-key notmuch-show-mode-map "u"
  (lambda ()
    "Toggle unread tag for message"
    (interactive)
    (notmuch-show-tag-message
     (if (member "unread" (notmuch-show-get-tags))
         "-unread" "+unread"))))

; Toggle star
(define-key notmuch-show-mode-map "S"
  (lambda ()
    "Toggle star for message"
    (interactive)
    (notmuch-show-tag-message
     (if (member "[Gmail].Starred" (notmuch-show-get-tags))
         "-[Gmail].Starred" "-flagged" "+[Gmail].Starred" "+flagged"))))

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
  (if (string= (s-chomp
                (shell-command-to-string "notmuch count tag:INBOX and tag:unread"))
               "0")
      nil
    t))

(provide 'msherry-mail)
