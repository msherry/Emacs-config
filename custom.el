;;; custom.el --- Holds values set by `customize'

;;; Commentary:

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separator-face ((t (:foreground "cornflower blue"))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "SpringGreen3")))))

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
 '(arduino-cli-default-fqbn "arduino:avr:uno")
 '(arduino-cli-default-port "/dev/cu.usbmodem1421")
 '(arduino-cli-verify t)
 '(arduino-cli-warnings 'all)
 '(arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino")
 '(auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
 '(auto-revert-verbose nil)
 '(blacken-line-length 100)
 '(csv-separators '("\11" ","))
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(dired-bind-jump nil t)
 '(display-time-default-load-average nil)
 '(display-time-format "")
 '(display-time-mail-face 'hi-blue)
 '(display-time-mail-function 'msherry-new-important-mail)
 '(display-time-mode t)
 '(ede-arduino-appdir "/Applications/Arduino.app/Contents/Java")
 '(elpy-eldoc-show-current-function nil)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv
     elpy-module-sane-defaults))
 '(elpy-project-ignored-directories
   '(".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" ".mypy_cache"))
 '(elpy-rpc-python-command "python2")
 '(elpy-rpc-timeout 10)
 '(elpy-test-runner 'elpy-test-nose-runner)
 '(epg-pinentry-mode 'loopback)
 '(eshell-prompt-function
   '(lambda nil
     (concat
      (if (and (boundp 'venv-current-name) venv-current-name)
          (concat "(" venv-current-name ") ")
        "")
      (user-login-name) "@" (system-name) ":"
      (abbreviate-file-name (eshell/pwd)) (msherry/git-branch)
      (if (= (user-uid) 0) " # " " $ "))))
 '(exec-path-from-shell-variables '("PATH" "MANPATH" "CARGO_HOME" "RUST_SRC_PATH" "GOPATH"))
 '(flycheck-checker-error-threshold nil)
 '(flycheck-checkers
   '(python-pycheckers lsp ada-gnat asciidoctor asciidoc awk-gawk
     bazel-build-buildifier bazel-module-buildifier bazel-starlark-buildifier
     bazel-workspace-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine
     chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint
     cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp
     emacs-lisp-checkdoc ember-template erlang-rebar3 erlang eruby-erubis
     eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test
     go-errcheck go-unconvert go-staticcheck groovy haml handlebars
     haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint
     javascript-jshint javascript-standard json-jsonlint json-python-json
     json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua
     markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl
     perl-perlcritic php php-phpmd php-phpcs processing proselint
     protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint
     python-flake8 python-pylint python-pycompile python-pyright python-mypy
     r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-standard
     ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala
     scala-scalastyle scheme-chicken scss-lint scss-stylelint
     sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh
     sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar
     terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint
     typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint
     yaml-jsyaml yaml-ruby yaml-yamllint))
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
   '(read-file-name-internal read-buffer todo-add-category
     gnus-emacs-completing-read gnus-iswitchb-completing-read grep-read-files
     magit-builtin-completing-read ess-completing-read Info-read-node-name
     tmm-prompt org-tags-completion-function ffap-read-file-or-url
     ffap-read-file-or-url-internal copy-region-as-kill))
 '(ido-enable-flex-matching t)
 '(ivy-use-selectable-prompt t)
 '(jabber-account-list
   '(("msherry@gmail.com" (:network-server . "talk.google.com") (:port . 5223)
      (:connection-type . ssl))))
 '(jabber-alert-message-hooks
   '(jabber-message-wave jabber-message-echo jabber-message-scroll))
 '(jabber-alert-message-wave "/System/Library/Sounds/Bottle.aiff")
 '(jabber-auto-reconnect t)
 '(jiralib-agile-page-size 500)
 '(jiralib-update-issue-fields-exclude-list '(components reporter priority))
 '(jiralib-url "https://futureprooftech.atlassian.net")
 '(lsp-diagnostics-provider :none)
 '(lsp-pylsp-plugins-jedi-signature-help-enabled nil)
 '(lsp-warn-no-matched-clients nil)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(magit-push-always-verify nil)
 '(magit-section-visibility-indicator nil)
 '(magit-tag-arguments '("--annotate"))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(max-lisp-eval-depth 217640)
 '(max-specpdl-size 36400 t)
 '(mm-inline-large-images 'resize)
 '(mm-text-html-renderer 'shr)
 '(notmuch-after-tag-hook
   '((lambda (&rest rest)
       (shell-command
        (concat "touch " (shell-quote-argument msherry-email-update-file-path))))))
 '(notmuch-archive-tags '("-INBOX"))
 '(notmuch-crypto-process-mime t)
 '(notmuch-fcc-dirs '(("msherry@gmail.com" . "msherry@gmail.com/sent")))
 '(notmuch-identities '("marc@futureproof.am" "msherry@gmail.com"))
 '(notmuch-saved-searches
   '((:name "inbox (futureproof)" :query "tag:INBOX AND tag:futureproof" :key
      [105])
     (:name "unread (futureproof)" :query
      "tag:unread AND tag:INBOX AND tag:futureproof" :key [117])
     (:name "inbox (personal)" :query "tag:INBOX and tag:personal" :key [108])
     (:name "unread (personal)" :query
      "tag:unread AND tag:INBOX AND tag:personal" :key [112])
     (:name "all mail (futureproof)" :query "tag:futureproof" :key [97])
     (:name "all mail (personal)" :query "tag:personal" :key [109])
     (:name "Sent mail" :query "tag:sent")))
 '(notmuch-search-oldest-first nil)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ocamlformat-enable 'enable-outside-detected-project)
 '(org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
 '(org-agenda-custom-commands
   '(("w" "Work schedule"
      ((tags "TODO=DONE" ((org-agenda-overriding-header "Done")))
       (tags-todo "WORK"
                  ((org-agenda-overriding-header "Sprints")
                   (org-super-agenda-groups '((:auto-property "sprint")))
                   (org-agenda-sorting-strategy
                    '((agenda todo-state-down time-up priority-down))))))
      nil)
     ("c" "Agenda and all unscheduled/everyday TODO's / unfiled"
      ((tags "REFILE" ((org-agenda-overriding-header "To refile")))
       (agenda ""
               ((org-super-agenda-groups
                 '((:log t) (:name "Schedule" :time-grid t)
                   (:name "To refile" :tag "REFILE")
                   (:name "Priority" :priority "A")
                   (:name "Overdue" :deadline past)
                   (:name "Due today" :deadline today)
                   (:name "Today" :scheduled today)
                   (:name "Due soon" :deadline future)
                   (:name "Home" :tag "HOME")
                   (:name "Motorcycle" :tag "MOTORCYCLE")
                   (:name "No deadline" :tag "WORK")))
                (org-agenda-sorting-strategy
                 '((agenda habit-down todo-state-down time-up priority-down
                    category-keep)
                   (todo priority-down category-keep)
                   (tags priority-down category-keep) (search category-keep)))))
       (tags "EVERYDAY"
             ((org-agenda-overriding-header "Every day")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'regexp
                 "\\^* .*:EVERYDAY\\|^\\*\\*\\*\\*"))))
       (todo ""
             ((org-agenda-overriding-header "Unscheduled TODOs")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))
       (tags "TOREAD"
             ((org-agenda-overriding-header "To read")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'regexp
                 "\\* To read.*:TOREAD:\\|.* DONE .*")))))
      nil)
     ("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes") (org-tags-match-list-sublevels t)))
     ("o"
      "Completed tasks older than 6 months (http://gnuru.org/article/1639/org-mode-find-all-done-items-older-than-2-months)"
      tags "CLOSED<\"<-6m>\"" nil)
     ("O" "Tasks completed within the past week" tags "CLOSED>=\"<-7d>\"" nil)
     ("u" "All open \"work\" TODOs (to refile)" tags-todo
      "WORK+ALLTAGS={\\(:FRAUDENG::$\\)\\|\\(:WORK::$\\)}"
      ((org-agenda-overriding-header "Untagged open work TODOs")) nil)
     ("U" "All untagged work TODOs (open/closed)" tags
      "WORK+ALLTAGS={\\(:FRAUDENG::$\\)\\|\\(:WORK::$\\)}"
      ((org-agenda-overriding-header "Untagged work TODOs (open/closed)")))
     ("N" "Notes" "NOTE"
      ((org-agenda-overriding-header "Notes") (org-tags-match-list-sublevels t)))))
 '(org-agenda-files '("~/.emacs.d/org" "~/.emacs.d/org/org-jira"))
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s") (timeline . "  % s") (todo . " %i %-12:c%l")
     (tags . " %i %-12:c") (search . " %i %-12:c")))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-span 'day)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-time-grid
   '((daily today) (800 1000 1200 1400 1600 1800 2000) "......" "----------------"))
 '(org-agenda-timegrid-use-ampm t)
 '(org-babel-clojure-backend 'cider)
 '(org-babel-load-languages
   '((C . t) (awk . t) (clojure . t) (dot . t) (emacs-lisp . t) (gnuplot . t)
     (lisp . t) (python . t) (ruby . t) (js . t) (shell . t) (sql . t)
     (scheme . t) (R . t)))
 '(org-capture-templates
   '(("h" "home TODO" entry (file+headline "~/.emacs.d/org/home.org" "Tasks")
      "** TODO %?\12%a\12")
     ("d" "Work TODO (DIGITALMGA)" entry
      (file+headline "~/.emacs.d/org/org-jira/DIGITALMGA.org"
       "* DIGITALMGA-Tickets")
      "** TODO %?\12:PROPERTIES:\12:assignee: marc\12:filename: DIGITALMGA\12:reporter: marc\12:type: Task\12:END:")
     ("w" "work TODO" entry (file+headline "~/.emacs.d/org/work.org" "Tasks")
      "** TODO %?\12 %a\12 " :empty-lines-after 1 :clock-resume t)
     ("p" "personal TODO" entry
      (file+headline "~/.emacs.d/org/personal.org" "Tasks")
      "** TODO %?\12 %a\12 " :empty-lines-after 1 :clock-in t :clock-resume t)
     ("l" "LLC TODO" entry (file+headline "~/.emacs.d/org/llc.org" "Tasks")
      "** TODO %?\12 %a\12 " :empty-lines-after 1 :clock-in t :clock-resume t)
     ("a" "HOA TODO" entry (file+headline "~/.emacs.d/org/hoa.org" "Tasks")
      "** TODO %?\12 %a\12 " :empty-lines-after 1 :clock-in t :clock-resume t)
     ("n" "note" entry (file "~/.emacs.d/org/refile.org")
      "* %? :NOTE:\12 %U\12 %a\12 " :empty-lines-after 1)
     ("m" "Meeting" entry (file+olp "~/.emacs.d/org/meetings.org" "Meetings")
      "* %? :MEETINGS:\12 " :empty-lines-after 1 :clock-in t :clock-resume t)
     ("b" "Purchase" entry (file+olp "~/.emacs.d/org/personal.org" "Purchases")
      "" :empty-lines-after 1)
     ("t" "(Work) task" entry (file+olp "~/.emacs.d/org/work.org" "Tasks")
      "** TODO %?\12DEADLINE: %t SCHEDULED: %t\12JIRA task\12\12Phab link\12\12Train\12\12%a"
      :empty-lines-after 1 :clock-in t :clock-resume t)))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist nil)
 '(org-clock-report-include-clocking-task t)
 '(org-confirm-babel-evaluate
   '(lambda (lang body) (message lang) (not (member lang '("dot")))))
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-export-with-sub-superscripts '{})
 '(org-habit-show-habits-only-for-today nil)
 '(org-image-actual-width '(300))
 '(org-indirect-buffer-display 'current-window)
 '(org-jira-jira-status-to-org-keyword-alist
   '(("To Do" . "TODO") ("In Progress" . "IN_PROGRESS")
     ("In Review" . "IN_REVIEW") ("Blocked" . "BLOCKED") ("Done" . "DONE")))
 '(org-jira-priority-to-org-priority-omit-default-priority t)
 '(org-jira-progress-issue-flow
   '(("To Do" . "In Progress") ("In Progress" . "In Review")
     ("In Review" . "Done")))
 '(org-jira-property-overrides nil)
 '(org-jira-working-dir "~/.emacs.d/org/org-jira")
 '(org-jira-worklog-sync-p nil)
 '(org-list-allow-alphabetical t)
 '(org-log-done 'time)
 '(org-mobile-agendas '("c"))
 '(org-mobile-files-exclude-regexp "-cal.org$")
 '(org-mobile-force-id-on-agenda-items nil)
 '(org-modules
   '(ol-elisp-symbol org-eval org-notify ol-notmuch org-bbdb org-bibtex
     org-docview org-gnus org-habit org-info org-irc org-mhe org-mouse
     org-rmail org-w3m))
 '(org-pomodoro-keep-killed-pomodoro-time t)
 '(org-refile-use-outline-path t)
 '(org-src-lang-modes
   '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
     ("asymptote" . asy) ("sqlite" . sql) ("calc" . fundamental) ("C" . c)
     ("cpp" . c++) ("C++" . c++) ("screen" . shell-script) ("shell" . sh)
     ("bash" . sh) ("dot" . graphviz-dot)))
 '(org-src-tab-acts-natively t)
 '(org-table-copy-increment nil)
 '(org-tag-alist '(("WORK" . 119) ("PERSONAL" . 112) ("HOME" . 104)))
 '(org-todo-keyword-faces '(("BLOCKED" . "#586e75") ("IN_QUEUE" . "#586e75")))
 '(org-use-sub-superscripts '{})
 '(package-selected-packages
   '(ac-geiser ack arduino-cli-mode arduino-mode auctex auto-minor-mode bazel-mode
     blacken cargo cider clojure-mode clojure-mode-extra-font-locking
     common-lisp-snippets counsel diff-hl dockerfile-mode dtrace-script-mode
     el2markdown elpy emojify ess esup exec-path-from-shell feature-mode
     find-find-in-project flx flx-ido flycheck-clojure flycheck-ocaml
     flycheck-package flycheck-pycheckers flycheck-rust forge fxrd-mode geiser
     geiser-guile geiser-racket git-modes gnuplot-mode go-mode
     graphviz-dot-mode groovy-mode helm-projectile httpcode
     ido-completing-read+ ivy jabber jedi json-mode julia-mode kotlin-mode
     latex-preview-pane lsp-jedi lsp-mode lsp-treemacs lsp-ui lua-mode magit
     magit-todos magithub markdown-preview-mode notmuch oauth2 ol-notmuch org
     org-agenda-property org-contrib org-jira org-mru-clock org-pomodoro
     org-super-agenda org-table-sticky-header ox-gfm package-lint paredit
     php-mode pinentry protobuf-mode puppet-mode pymacs python-coverage
     python-mode python-pytest racer racket-mode rainbow-mode realgud-ipdb
     rmsbolt rust-mode s salt-mode sass-mode slime smex smtpmail-multi
     solarized-theme string-inflection suggest swift-mode tagedit thrift
     tickscript-mode tuareg virtualenv which-key window-numbering yaml-mode
     yasnippet-snippets))
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-modes
   '("erc-mode" "help-mode" "completion-list-mode" "Buffer-menu-mode"
     "gnus-.*-mode" "occur-mode" "graphviz-dot-mode"))
 '(py-underscore-word-syntax-p nil)
 '(python-shell-interpreter "python")
 '(racer-rust-src-path nil)
 '(rust-format-on-save t)
 '(safe-local-variable-values
   '((eval emojify-mode nil) (eval org-table-sticky-header-mode t)
     (time-stamp-active . t) (elpy-test-runner . elpy-test-pytest-runner)
     (blacken-line-length . 100) (magit-todos-depth . 1)
     (org-confirm-babel-evaluate)
     (arduino-cli-default-fqbn . "arduino:avr:nano:cpu=atmega328old")
     (arduino-cli-default-port . "/dev/cu.wchusbserial1410")
     (arduino-cli-default-port . "/dev/cu.wchusbserial1420")
     (arduino-cli-default-port . "/dev/cu.usbmodem1421")
     (arduino-cli-default-fqbn . "arduino:avr:uno")
     (arduino-cli-default-port . "/dev/cu/wchusbserial1420")
     (arduino-cli-default-port . "/dev/cu.usbmodel1411")
     (arduino-cli-default-fqbn . "arduino:avr:nano")
     (arduino-cli-default-port . "/dev/cu.usbmodel1421") (eval org-jira-mode t)
     (jiralib-url . "https://jira.team.affirm.com")
     (jedi:environment-root .
      "/Users/marcsherry/src/all-the-things/deployable/monolith/src/.venv")
     (rust--format-args quote
      ("--config-path" "/Users/msherry/src/client/rust/nucleus/.rustfmt.toml"))
     (flycheck-checker . go-lint) (tickscript-kapacitor-version . "1.3")
     (tickscript-kapacitor-version . "1.4")
     (tickscript-series-name . "medians_dev")
     (tickscript-series-dbrp . "apogee.autogen")
     (tickscript-kapacitor-url . http://localhost:9092)
     (tickscript-kapacitor-url . http://localhost:5092)
     (tickscript-series-type . "stream")
     (tickscript-series-dbrp . "desktop_client.default")
     (tickscript-series-type . "batch") (tickscript-series-name . "medians")))
 '(smtpmail-multi-accounts
   '((personal "msherry@gmail.com" "smtp.gmail.com" 587 header starttls nil nil
      nil)
     (futureproof "marc@futureproof.am" "smtp.gmail.com" 587 header starttls
      nil nil nil)))
 '(smtpmail-multi-associations '(("msherry@gmail.com" personal)))
 '(tickscript-add-extra-graph-options t)
 '(tramp-default-method "ssh")
 '(tramp-default-method-alist
   '((nil "\\`\\(anonymous\\|ftp\\)\\'" "ftp") ("\\`ftp\\." nil "ftp")
     ("\\`\\(127\\.0\\.0\\.1\\|::1\\|Marc\\.S-MBPro\\|localhost6?\\)\\'"
      "\\`root\\'" "su")
     ("raven-\\.*" nil "tsh")))
 '(tramp-syntax 'default nil (tramp)))

(provide 'custom)
;;; custom.el ends here
