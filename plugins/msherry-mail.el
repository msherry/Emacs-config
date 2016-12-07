;;; Mail sending
(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;; Browsing
(require 'notmuch)
(require 's)

(setq mm-text-html-renderer 'w3m)
(setq notmuch-fcc-dirs nil)             ; Gmail saves sent mails by itself

(global-set-key (kbd "C-c m") `notmuch)

;;; Tagging keybindings

;; Open links in emails -- super hacky, it uses org-mode's functionality
(define-key notmuch-show-mode-map (kbd  "C-c C-o")
  (lambda ()
    "Open link at point"
    (interactive)
    (org-open-link-from-string (url-get-url-at-point))))

;; Archive mail in search mode
(define-key notmuch-search-mode-map "y"
      (lambda (&optional beg end)
        "Remove thread from inbox"
        (interactive (notmuch-search-interactive-region))
        (notmuch-search-tag (list "-INBOX") beg end)
        (notmuch-refresh-this-buffer)))

;; Archive mail in show mode
(define-key notmuch-show-mode-map "y"
  (lambda ()
    "Remove thread from inbox, return to search mode"
    (interactive)
    (notmuch-show-tag-message "-INBOX")
    (notmuch-bury-or-kill-this-buffer)
    (notmuch-refresh-this-buffer)))

;; Toggle unread in search mode
(define-key notmuch-search-mode-map "u"
  (lambda ()
    "Toggle unread tag for message"
    (interactive)
    (notmuch-search-tag
     (if (member "unread" (notmuch-search-get-tags))
         "-unread" "+unread"))))

;; Toggle unread in show mode
(define-key notmuch-show-mode-map "u"
  (lambda ()
    "Toggle unread tag for message"
    (interactive)
    (notmuch-show-tag-message
     (if (member "unread" (notmuch-show-get-tags))
         "-unread" "+unread"))))


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
