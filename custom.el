;;; custom.el --- Holds values set by `customize'

;;; Commentary:

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(button ((t (:foreground "#b58900" :underline t))))
 '(csv-separator-face ((t (:foreground "cornflower blue"))))
 '(diff-added ((t (:background "#002b36" :foreground "green4"))))
 '(diff-refine-added ((t (:inherit diff-refine-change :background "dark green"))))
 '(diff-refine-removed ((t (:inherit diff-refine-change :background "red4"))))
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "#ff00ff" :foreground "blue"))))
 '(ediff-current-diff-C ((t (:background "#888833" :foreground "black"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "Grey" :foreground "black"))))
 '(error ((t (:foreground "Coral"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "SpringGreen3"))))
 '(org-habit-ready-face ((t (:background "green4" :foreground "#859900"))))
 '(org-habit-ready-future-face ((t (:background "green4"))))
 '(region ((((class color) (min-colors 24)) (:background "#00ffff"))))
 '(warning ((t (:background "#a4caff")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-command "ag --ignore tests ")
 '(ack-defaults-function 'ack-legacy-defaults)
 '(appt-delete-window-function (lambda nil))
 '(appt-disp-window-function 'msherry/appt-disp-window)
 '(appt-display-format 'window)
 '(appt-display-interval 5)
 '(appt-message-warning-time 10)
 '(arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino")
 '(auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
 '(auto-revert-verbose nil)
 '(csv-separators '("	" ","))
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(dired-bind-jump nil)
 '(display-time-default-load-average nil)
 '(display-time-format "")
 '(display-time-mail-face 'hi-blue)
 '(display-time-mail-function 'msherry-new-important-mail)
 '(display-time-mode t)
 '(ede-arduino-appdir "/Applications/Arduino.app/Contents/Java")
 '(elpy-eldoc-show-current-function nil)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-sane-defaults))
 '(elpy-project-ignored-directories
   '(".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" ".mypy_cache"))
 '(elpy-rpc-python-command "python2")
 '(elpy-rpc-timeout 10)
 '(elpy-test-runner 'elpy-test-nose-runner)
 '(epg-pinentry-mode 'loopback)
 '(exec-path-from-shell-variables '("PATH" "MANPATH" "CARGO_HOME" "RUST_SRC_PATH" "GOPATH"))
 '(flycheck-checker-error-threshold nil)
 '(flycheck-display-errors-delay 0.15)
 '(flycheck-flake8-maximum-line-length 120)
 '(flycheck-global-modes '(not org-mode org-agenda-mode))
 '(flycheck-highlighting-mode 'lines)
 '(flycheck-pycheckers-checkers '(pylint pep8 mypy2 mypy3 bandit))
 '(flycheck-pycheckers-enable-codes '("W0613"))
 '(flycheck-pycheckers-max-line-length 100)
 '(flycheck-rust-check-tests t)
 '(gc-cons-threshold 100000000)
 '(git-commit-summary-max-length 79)
 '(global-eldoc-mode nil)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(ido-cr+-allow-list '(org-agenda-refile org-refile))
 '(ido-cr+-disable-list
   '(read-file-name-internal read-buffer todo-add-category gnus-emacs-completing-read gnus-iswitchb-completing-read grep-read-files magit-builtin-completing-read ess-completing-read Info-read-node-name tmm-prompt org-tags-completion-function ffap-read-file-or-url ffap-read-file-or-url-internal copy-region-as-kill))
 '(jabber-account-list
   '(("msherry@gmail.com"
      (:network-server . "talk.google.com")
      (:port . 5223)
      (:connection-type . ssl))))
 '(jabber-alert-message-hooks
   '(jabber-message-wave jabber-message-echo jabber-message-scroll))
 '(jabber-alert-message-wave "/System/Library/Sounds/Bottle.aiff")
 '(jabber-auto-reconnect t)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(magit-push-always-verify nil)
 '(magit-section-visibility-indicator nil)
 '(magit-tag-arguments '("--annotate"))
 '(mm-inline-large-images 'resize)
 '(mm-text-html-renderer 'shr)
 '(notmuch-after-tag-hook
   '((lambda
         (&rest rest)
       (shell-command
        (concat "touch "
                (shell-quote-argument msherry-email-update-file-path))))))
 '(notmuch-archive-tags '("-INBOX"))
 '(notmuch-crypto-process-mime t)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:INBOX" :key "i")
     (:name "unread" :query "tag:unread AND tag:INBOX" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "Identity triage" :query "tag:identity_triage")
     (:name "Riskeng triage" :query "tag:unread AND tag:riskeng_triage")))
 '(notmuch-search-oldest-first nil)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ocamlformat-enable 'enable-outside-detected-project)
 '(org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
 '(org-agenda-custom-commands
   '(("c" "Agenda and all unscheduled/everyday TODO's / unfiled"
      ((agenda ""
               ((org-super-agenda-groups
                 '((:log t)
                   (:name "Schedule" :time-grid t)
                   (:name "Priority" :priority "A")
                   (:name "Overdue" :deadline past)
                   (:name "Due today" :deadline today)
                   (:name "Today" :scheduled today)
                   (:name "Due soon" :deadline future)
                   (:name "No deadline" :tag "WORK")))
                (org-agenda-sorting-strategy
                 '((agenda habit-down todo-state-down time-up priority-down category-keep)
                   (todo priority-down category-keep)
                   (tags priority-down category-keep)
                   (search category-keep)))))
       (tags "EVERYDAY"
             ((org-agenda-overriding-header "Every day")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'regexp "\\^* .*:EVERYDAY\\|^\\*\\*\\*\\*"))))
       (todo ""
             ((org-agenda-overriding-header "Unscheduled TODOs")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))
       (tags "TOREAD"
             ((org-agenda-overriding-header "To read")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'regexp "\\* To read.*:TOREAD:"))))
       (tags "REFILE"
             ((org-agenda-overriding-header "To refile"))))
      nil)
     ("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("o" "Completed tasks older than 6 months (http://gnuru.org/article/1639/org-mode-find-all-done-items-older-than-2-months)" tags "CLOSED<\"<-6m>\"" nil)
     ("w" "Tasks completed within the past week" tags "CLOSED>=\"<-7d>\"" nil)
     ("u" "All open \"work\" TODOs (to refile)" tags-todo "WORK+ALLTAGS={\\(:FRAUDENG:$\\)\\|\\(:WORK:$\\)}"
      ((org-agenda-overriding-header "Untagged open work TODOs"))
      nil)
     ("U" "All untagged work TODOs (open/closed)" tags "WORK+ALLTAGS={\\(:FRAUDENG:$\\)\\|\\(:WORK:$\\)}"
      ((org-agenda-overriding-header "Untagged work TODOs (open/closed)")))))
 '(org-agenda-files '("~/.emacs.d/org"))
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (timeline . "  % s")
     (todo . " %i %-12:c%l")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-span 'day)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-sticky t)
 '(org-agenda-time-grid
   '((daily today)
     (800 1000 1200 1400 1600 1800 2000)
     "......" "----------------"))
 '(org-agenda-timegrid-use-ampm t)
 '(org-babel-clojure-backend 'cider)
 '(org-babel-load-languages
   '((C . t)
     (awk . t)
     (clojure . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (lisp . t)
     (python . t)
     (ruby . t)
     (js . t)
     (shell . t)
     (sql . t)))
 '(org-capture-templates
   '(("w" "work TODO" entry
      (file+headline "~/.emacs.d/org/work.org" "Tasks")
      "** TODO %?
 %a
 " :empty-lines-after 1 :clock-resume t)
     ("p" "personal TODO" entry
      (file+headline "~/.emacs.d/org/personal.org" "Tasks")
      "** TODO %?
 %a
 " :empty-lines-after 1 :clock-in t :clock-resume t)
     ("l" "LLC TODO" entry
      (file+headline "~/.emacs.d/org/llc.org" "Tasks")
      "** TODO %?
 %a
 " :empty-lines-after 1 :clock-in t :clock-resume t)
     ("h" "HOA TODO" entry
      (file+headline "~/.emacs.d/org/hoa.org" "Tasks")
      "** TODO %?
 %a
 " :empty-lines-after 1 :clock-in t :clock-resume t)
     ("n" "note" entry
      (file "~/.emacs.d/org/refile.org")
      "* %? :NOTE:
 %U
 %a
 " :empty-lines-after 1)
     ("m" "Meeting" entry
      (file+olp "~/.emacs.d/org/work.org" "Meetings")
      "* %? :MEETINGS:
 " :empty-lines-after 1 :clock-in t :clock-resume t)
     ("b" "Purchase" entry
      (file+olp "~/.emacs.d/org/personal.org" "Purchases")
      "" :empty-lines-after 1)
     ("t" "(Work) task" entry
      (file+olp "~/.emacs.d/org/work.org" "Tasks")
      "** TODO %?
DEADLINE: %t SCHEDULED: %t
JIRA task

Phab link

Train

%a" :empty-lines-after 1 :clock-in t :clock-resume t)))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist nil)
 '(org-clock-report-include-clocking-task t)
 '(org-confirm-babel-evaluate
   '(lambda
     (lang body)
     (message lang)
     (not
      (member lang
       '("dot")))))
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-export-with-sub-superscripts '{})
 '(org-habit-show-habits-only-for-today nil)
 '(org-image-actual-width '(300))
 '(org-indirect-buffer-display 'current-window)
 '(org-list-allow-alphabetical t)
 '(org-log-done 'time)
 '(org-mobile-agendas '("c"))
 '(org-mobile-files-exclude-regexp "-cal.org$")
 '(org-mobile-force-id-on-agenda-items nil)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-mouse org-rmail org-w3m org-notmuch))
 '(org-pomodoro-keep-killed-pomodoro-time t)
 '(org-refile-use-outline-path t)
 '(org-src-lang-modes
   '(("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)
     ("shell" . sh)
     ("bash" . sh)
     ("dot" . graphviz-dot)))
 '(org-src-tab-acts-natively t)
 '(org-table-copy-increment nil)
 '(org-todo-keyword-faces '(("BLOCKED" . "#586e75") ("IN_QUEUE" . "#586e75")))
 '(org-use-sub-superscripts '{})
 '(package-selected-packages
   '(ac-geiser ack arduino-mode auctex bazel-mode blacken cargo cider clojure-mode clojure-mode-extra-font-locking common-lisp-snippets diff-hl dockerfile-mode dtrace-script-mode el2markdown elpy emojify ess esup exec-path-from-shell feature-mode flx-ido flycheck-clojure flycheck-ocaml flycheck-package flycheck-pycheckers flycheck-rust fxrd-mode geiser gitignore-mode go-mode graphviz-dot-mode groovy-mode helm-projectile httpcode ido-completing-read+ jabber jedi json-mode kotlin-mode latex-preview-pane magit magithub markdown-preview-mode notmuch oauth2 org-agenda-property org-jira org-mru-clock org-plus-contrib org-pomodoro org-super-agenda ox-gfm package-lint paredit php-mode pinentry projectile protobuf-mode puppet-mode pymacs python-mode racer rainbow-mode rmsbolt rust-mode s sass-mode slime solarized-theme suggest swift-mode tagedit thrift tickscript-mode tuareg virtualenv which-key window-numbering yaml-mode yasnippet-snippets))
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-modes
   '("erc-mode" "help-mode" "completion-list-mode" "Buffer-menu-mode" "gnus-.*-mode" "occur-mode" "graphviz-dot-mode"))
 '(python-shell-interpreter "python")
 '(racer-rust-src-path nil)
 '(rust-format-on-save t)
 '(safe-local-variable-values
   '((eval org-jira-mode t)
     (jiralib-url . "https://jira.team.affirm.com")
     (jedi:environment-root . "/Users/marcsherry/src/all-the-things/deployable/monolith/src/.venv")
     (rust--format-args quote
      ("--config-path" "/Users/msherry/src/client/rust/nucleus/.rustfmt.toml"))
     (flycheck-checker . go-lint)
     (tickscript-kapacitor-version . "1.3")
     (tickscript-kapacitor-version . "1.4")
     (tickscript-series-name . "medians_dev")
     (tickscript-series-dbrp . "apogee.autogen")
     (tickscript-kapacitor-url . http://localhost:9092)
     (tickscript-kapacitor-url . http://localhost:5092)
     (tickscript-series-type . "stream")
     (tickscript-series-dbrp . "desktop_client.default")
     (tickscript-series-type . "batch")
     (tickscript-series-name . "medians")))
 '(tickscript-add-extra-graph-options t)
 '(tramp-default-method "ssh"))

(provide 'custom)
;;; custom.el ends here
