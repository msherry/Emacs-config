;;; geiser-guile.el --- Guile and Geiser talk to each other  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Jose Antonio Ortega Ruiz
;; Start date: Sun Mar 08, 2009 23:03

;; Author: Jose Antonio Ortega Ruiz (jao@gnu.org)
;; Maintainer: Jose Antonio Ortega Ruiz (jao@gnu.org)
;; Keywords: languages, guile, scheme, geiser
;; Homepage: https://gitlab.com/emacs-geiser/guile
;; Package-Requires: ((emacs "26.1") (transient "0.3") (geiser "0.28.1"))
;; SPDX-License-Identifier: BSD-3-Clause
;; Package-Version: 20240920.35
;; Package-Revision: a0f111f8dedd

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package extends the `geiser' core package to support GNU
;; Guile.


;;; Code:

(require 'geiser-connection)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-repl)
(require 'geiser-debug)
(require 'geiser-impl)
(require 'geiser-base)
(require 'geiser-eval)
(require 'geiser-edit)
(require 'geiser-log)
(require 'geiser)

(require 'transient)
(require 'compile)
(require 'info-look)
(require 'tramp)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))


;;; Customization

(defgroup geiser-guile nil
  "Customization for Geiser's Guile flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-guile-binary
    (cond ((eq system-type 'windows-nt) "guile.exe")
          ((eq system-type 'darwin) "guile")
          (t "guile"))
  "Name to use to call the Guile executable when starting a REPL."
  :type '(choice string (repeat string)))

(geiser-custom--defcustom geiser-guile-load-path nil
  "A list of paths to be added to Guile's load path when it's started.
The paths are added to both %`load-path' and %load-compiled path,
and only if they are not already present.  This variable is a
good candidate for an entry in your project's .dir-locals.el."
  :type '(repeat file))

(geiser-custom--defcustom geiser-guile-init-file "~/.guile-geiser"
  "Initialization file with user code for the Guile REPL.
If all you want is to load ~/.guile, set
`geiser-guile-load-init-file' instead."
  :type 'string)

(define-obsolete-variable-alias
  'geiser-guile-load-init-file-p 'geiser-guile-load-init-file "0.26.2")

(geiser-custom--defcustom geiser-guile-load-init-file nil
  "Whether to load ~/.guile when starting Guile.
Note that, due to peculiarities in the way Guile loads its init
file, using `geiser-guile-init-file' is not equivalent to setting
this variable to t."
  :type 'boolean)

(define-obsolete-variable-alias
  'geiser-guile-use-declarative-modules-p 'geiser-guile-use-declarative-modules
  "0.26.2")

(geiser-custom--defcustom geiser-guile-use-declarative-modules nil
  "Whether Guile should use \"declarative\" modules limiting mutability.
When set to `t', Guile will enforce immutable bindings in
exported modules."
  :type 'boolean
  :link '(info-link "(guile) Declarative Modules"))

(geiser-custom--defcustom geiser-guile-debug-backwards-backtrace t
  "Whether to configure backtraces using the \\='backwards ordering."
  :type 'boolean)

(geiser-custom--defcustom geiser-guile-debug-terminal-width 999
  "Maximum number of columns shown in backtraces.
Normally, you'd want a big value here so that messages are not
truncated.  Set to a negative value if you prefer that geiser
does not set it on startup."
  :type 'integer)

(define-obsolete-variable-alias
  'geiser-guile-debug-show-bt-p 'geiser-guile-debug-show-bt "0.26.2")

(geiser-custom--defcustom geiser-guile-debug-show-bt t
  "Whether to automatically show a full backtrace when entering the debugger.
If nil, only the last frame is shown."
  :type 'boolean)

(define-obsolete-variable-alias
  'geiser-guile-debug-show-full-bt-p 'geiser-guile-debug-show-full-bt "0.26.2")

(geiser-custom--defcustom geiser-guile-debug-show-full-bt t
  "Whether to show full backtraces in the debugger, including local variables."
  :type 'boolean)

(define-obsolete-variable-alias
  'geiser-guile-show-debug-help-p 'geiser-guile-show-debug-help "0.26.2")

(geiser-custom--defcustom geiser-guile-show-debug-help t
  "Whether to show brief help in the echo area when entering the debugger."
  :type 'boolean)

(geiser-custom--defcustom geiser-guile-warning-level 'medium
  "Verbosity of the warnings reported by Guile.

You can either choose one of the predefined warning sets, or
provide a list of symbols identifying the ones you want.  Possible
choices are arity-mismatch, unbound-variable, unused-variable and
unused-toplevel.  Unrecognised symbols are ignored.

The predefined levels are:

  - Medium: arity-mismatch, unbound-variable, format
  - High: arity-mismatch, unbound-variable, unused-variable, format
  - None: no warnings

Changes to the value of this variable will automatically take
effect on new REPLs.  For existing ones, use the command
\\[geiser-guile-update-warning-level]."
  :type '(choice (const :tag "Medium (arity and unbound vars)" medium)
                 (const :tag "High (also unused vars)" high)
                 (const :tag "No warnings" none)
                 (repeat :tag "Custom" symbol)))

(geiser-custom--defcustom geiser-guile-extra-keywords nil
  "Extra keywords highlighted in Guile scheme buffers."
  :type '(repeat string))

(define-obsolete-variable-alias
  'geiser-guile-case-sensitive-p 'geiser-guile-case-sensitive "0.26.2")

(geiser-custom--defcustom geiser-guile-case-sensitive t
  "Non-nil means keyword highlighting is case-sensitive."
  :type 'boolean)

(define-obsolete-variable-alias
  'geiser-guile-manual-lookup-other-window-p
  'geiser-guile-manual-lookup-other-window "0.26.2")

(geiser-custom--defcustom geiser-guile-manual-lookup-other-window nil
  "Non-nil means pop up the Info buffer in another window."
  :type 'boolean)

(geiser-custom--defcustom geiser-guile-manual-lookup-nodes
    '("Guile" "guile-2.0")
  "List of info nodes that, when present, are used for manual lookups."
  :type '(repeat string))

(make-obsolete-variable 'geiser-guile-manual-lookup-nodes
                        'geiser-guile-manual-lookup-alist
                        "0.28.2")

(geiser-custom--defcustom geiser-guile-manual-lookup-indices
    '("R5RS Index" "Concept Index" "Procedure Index" "Variable Index")
  "List of info index nodes that, when present, are used for manual lookups."
  :type '(repeat string))

(make-obsolete-variable 'geiser-guile-manual-lookup-indices
                        'geiser-guile-manual-lookup-alist
                        "0.28.2")

(geiser-custom--defcustom geiser-guile-manual-lookup-alist
    (append (mapcar (lambda (x) (cons x geiser-guile-manual-lookup-indices))
                    geiser-guile-manual-lookup-nodes)
            '(("r5rs" . ("Index"))))
  "Alist of Info nodes and their indices that are used for manual lookups.
Each element looks like (NODE . INDICES).  NODE is an Info node
and INDICES is a list of index nodes corresponding to NODE."
  :type '(alist :key-type string :value-type (repeat string))
  :set (lambda (sym val)
         "Update `info-lookup-alist' based on VAL.
Also, update the toplevel default value of SYM to VAL."
         (let ((nrx "^[       ]+-+ [^:]+:[    ]*")
               (drx "\\b")
               (spec))
           (dolist (row val spec)
             (when-let ((file (car row))
                        ((Info-find-file file t))
                        (indices (cdr row)))
               (dolist (ix indices)
                 (push (list (format "(%s)%s" file ix) nil nrx drx) spec))))
           (info-lookup-add-help :topic 'symbol
                                 :mode 'geiser-guile-mode
                                 :ignore-case nil
                                 :regexp "[^()`',\"        \n]+"
                                 :doc-spec spec))
         (set-default-toplevel-value sym val)))

(geiser-custom--defcustom geiser-guile-doc-process-texinfo nil
  "Non-nil means try to convert docstrings from texinfo into plain-text.

Changes to the value of this variable will automatically take
effect on new REPLs.  For existing ones, use the command
\\[geiser-guile-update-doc-process-texinfo]."
  :type 'boolean)


;;; REPL support

(defun geiser-guile--binary ()
  "Return the name of the Guile binary to execute."
  (if (listp geiser-guile-binary)
      (car geiser-guile-binary)
    geiser-guile-binary))

(defvar geiser-guile-scheme-dir
  (expand-file-name "src" (file-name-directory load-file-name))
  "Directory where the Guile scheme geiser modules are installed.")

(defvar-local geiser-guile-scheme-local-dir
    nil
  "Location for scm files to communicate using REPL that are local to process.

When using Tramp buffers, the guile modules are not local. They'll be stored in
this location for further cleanup.")

(defun geiser-guile--remote-copy (source-path target-path)
  "Copy source-path to target-path ensuring symlinks are resolved."
  ;; when using `straight', guile scripts that need to be evaluated will be
  ;; symlinks
  ;; `copy-directory' will copy broken symlinks
  ;; so we manually copy them to avoid broken symlinks in remote host
  (cond ((file-symlink-p source-path)
         (geiser-guile--remote-copy (file-truename source-path) target-path))
        ((file-directory-p source-path)
         (unless (file-directory-p target-path) (make-directory target-path t))
         (let ((dest (file-name-as-directory target-path)))
           (dolist (f (seq-difference (directory-files source-path) '("." "..")))
             (geiser-guile--remote-copy (expand-file-name f source-path)
                                        (expand-file-name f dest)))))
        (t (cl-assert (file-regular-p source-path))
           (copy-file source-path target-path))))

(defun geiser-guile-ensure-scheme-dir ()
  "Maybe setup and return dir for Guile scheme geiser modules.

If using a remote Tramp buffer, this function will copy the modules to a
temporary location in the remote server and the return it.
Else, will just return `geiser-guile-scheme-dir'."
  (cond ((not (and (fboundp 'tramp-tramp-file-p)
                   (tramp-tramp-file-p default-directory)))
         geiser-guile-scheme-dir)
        (geiser-guile-scheme-local-dir) ;; remote files are already there
        (t
         (let* ((temporary-file-directory (temporary-file-directory))
                (remote-temp-dir (make-temp-file "emacs-geiser-guile" t)))
           (message "Setting up Tramp Guile REPL...")
           (let ((inhibit-message t)) ;; prevent "Copying … to … " from dired
             (geiser-guile--remote-copy
              geiser-guile-scheme-dir
              (concat (file-name-as-directory remote-temp-dir)
                      (file-name-nondirectory
                       (directory-file-name geiser-guile-scheme-dir)))))
           ;; return the directory name as local to (remote) process
           (setq geiser-guile-scheme-local-dir
                 (concat (file-name-as-directory
                          (file-local-name remote-temp-dir))
                         (file-name-nondirectory geiser-guile-scheme-dir)))))))

(defvar geiser-guile--conn-address nil)

(defun geiser-guile--get-connection-address (&optional new)
  "The path to the UNIX socket to talk to Guile in a connection.
Unused for now."
  (when new
    (setq geiser-guile--conn-address (make-temp-name "/tmp/geiser-guile-")))
  geiser-guile--conn-address)

(defun geiser-guile--parameters ()
  "Return a list with all parameters needed to start Guile.
This function uses `geiser-guile-init-file' if it exists."
  (let ((init-file (and (stringp geiser-guile-init-file)
                        (expand-file-name
                         (concat
                          (file-remote-p default-directory)
                          geiser-guile-init-file))))
        (c-flags (when geiser-guile--conn-address
                   `(,(format "--listen=%s"
                              (geiser-guile--get-connection-address t)))))
        (q-flags (and (not geiser-guile-load-init-file) '("-q"))))
    `(,@(and (listp geiser-guile-binary) (cdr geiser-guile-binary))
      ,@q-flags "-L" ,(geiser-guile-ensure-scheme-dir) ,@c-flags
      ,@(apply 'append (mapcar (lambda (p) (list "-L" p))
                               geiser-guile-load-path))
      ,@(and init-file (file-readable-p init-file)
             (list "-l" (file-local-name init-file))))))

(defconst geiser-guile--prompt-regexp "^[^@(\n]+@([^)]*)> ")
(defconst geiser-guile--debugger-prompt-regexp
  "^[^@(\n]+@([^)]*?) \\[\\([0-9]+\\)\\]> ")

(defconst geiser-guile--clean-rx
  (format "\\(%s\\)\\|\\(^\\$[0-9]+ = [^\n]+$\\)\\|%s"
          (geiser-con--combined-prompt geiser-guile--prompt-regexp
                                       geiser-guile--debugger-prompt-regexp)
          "\\(\nEntering a new prompt.  Type `,bt' for [^\n]+\\.$\\)"))


;;; Evaluation support
(defsubst geiser-guile--linearize-args (args)
  "Concatenate the list ARGS."
  (mapconcat 'identity args " "))

(defun geiser-guile--debug-cmd (args)
  (let ((args (if (and geiser-guile-debug-show-full-bt
                       (string= (car args) "backtrace"))
                  '("backtrace" "#:full?" "#t")
                args)))
    (concat "," (geiser-guile--linearize-args args) "\n\"\"")))

(defun geiser-guile--geiser-procedure (proc &rest args)
  "Transform PROC in string for a scheme procedure using ARGS."
  (cl-case proc
    ((eval compile) (format ",geiser-eval %s %s%s"
                            (or (car args) "#f")
                            (geiser-guile--linearize-args (cdr args))
                            (if (cddr args) "" " ()")))
    ((load-file compile-file) (format ",geiser-load-file %s" (car args)))
    ((no-values) ",geiser-no-values")
    ((debug) (geiser-guile--debug-cmd args))
    (t (format "ge:%s (%s)" proc (geiser-guile--linearize-args args)))))

(defun geiser-guile--clean-up-output (str)
  (let ((msg (when (string-match geiser-guile--debugger-prompt-regexp str)
               (format "\n[Debugging level: %s]" (match-string 1 str)))))
    (concat (replace-regexp-in-string geiser-guile--clean-rx "" str) msg)))

(defconst geiser-guile--module-re
  "(define-module +\\(([^)]+)\\)")

(defconst geiser-guile--library-re
  "(\\(?:define-\\)?library[[:blank:]\n]+\\(([^)]+)\\)")

(defun geiser-guile--get-module (&optional module)
  "Find current buffer's module using MODULE as a hint."
  (cond ((null module)
         (save-excursion
           (geiser-syntax--pop-to-top)
           (if (or (re-search-backward geiser-guile--module-re nil t)
                   (re-search-backward geiser-guile--library-re nil t)
                   (re-search-forward geiser-guile--module-re nil t)
                   (re-search-forward geiser-guile--library-re nil t))
               (geiser-guile--get-module (match-string-no-properties 1))
             :f)))
        ((listp module) module)
        ((stringp module)
         (condition-case nil
             (car (geiser-syntax--read-from-string module))
           (error :f)))
        (t :f)))

(defun geiser-guile--module-cmd (module fmt &optional def)
  "Use FMT to format a change to MODULE, with default DEF."
  (when module
    (let* ((module (geiser-guile--get-module module))
           (module (cond ((or (null module) (eq module :f)) def)
                         (t (format "%s" module)))))
      (and module (format fmt module)))))

(defun geiser-guile--import-command (module)
  "Format a REPL command to use MODULE."
  (geiser-guile--module-cmd module ",use %s"))

(defun geiser-guile--enter-command (module)
  "Format a REPL command to enter MODULE."
  (geiser-guile--module-cmd module ",m %s" "(guile-user)"))


(defun geiser-guile--exit-command ()
  "Format a REPL command to quit."
  ",q")

(defun geiser-guile--symbol-begin (module)
  "Find beginning of symbol in the context of MODULE."
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))


;;; Compilation shell regexps

(defconst geiser-guile--path-rx "^In \\([^:\n ]+\\):\n")

(defconst geiser-guile--rel-path-rx "^In +\\([^/\n: ]+\\):\n")

(defvar geiser-guile--file-cache (make-hash-table :test 'equal)
  "Internal cache.")

(defun geiser-guile--find-file (file)
  (or (gethash file geiser-guile--file-cache)
      (with-current-buffer (or geiser-debug--sender-buffer (current-buffer))
        (when-let (r geiser-repl--repl)
          (with-current-buffer r
            (geiser-eval--send/result `(:eval (:ge find-file ,file))))))))

(defun geiser-guile--resolve-file (file)
  "Find the given FILE, if it's indeed a file."
  (when (and (stringp file)
             (not (member file
                          '("socket" "stdin" "unknown file" "current input"))))
    (message "Resolving %s" file)
    (cond ((file-name-absolute-p file) file)
          (t (when-let (f (geiser-guile--find-file file))
               (puthash file f geiser-guile--file-cache))))))

(defun geiser-guile--resolve-file-x ()
  "Check if last match contain a resolvable file."
  (let ((f (geiser-guile--resolve-file (match-string-no-properties 1))))
    (and (stringp f) (list f))))


;;; Error display and debugger

(defun geiser-guile--set-up-error-links ()
  (setq-local compilation-error-regexp-alist
              `((,geiser-guile--path-rx geiser-guile--resolve-file-x)
                ("^  +\\([0-9]+\\):\\([0-9]+\\)" nil 1 2)
                ("^\\(/.*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))
  (font-lock-add-keywords nil
                          `((,geiser-guile--path-rx 1 compilation-error-face))))

(defun geiser-guile-debug--send-dbg (thing)
  (geiser-eval--send/wait (cons :debug (if (listp thing) thing (list thing)))))

(defun geiser-guile-debug--debugger-display (thing ret)
  (geiser-debug--display-retort (format ",%s" thing)
                                ret
                                (geiser-eval--retort-result-str ret nil)))

(defun geiser-guile-debug--send-to-repl (thing)
  (unless (geiser-debug-active-p) (error "Debugger not active"))
  (save-window-excursion
    (with-current-buffer geiser-debug--sender-buffer
      (when-let (ret (geiser-guile-debug--send-dbg thing))
        (geiser-guile-debug--debugger-display thing ret)))))

(defun geiser-guile-debug-quit ()
  "Quit the current debugging session level."
  (interactive)
  (geiser-guile-debug--send-to-repl 'quit))

(defun geiser-guile-debug-show-backtrace ()
  "Quit the current debugging session level."
  (interactive)
  (geiser-guile-debug--send-to-repl 'backtrace))

(defun geiser-guile-debug-show-locals ()
  "Show local variables."
  (interactive)
  (geiser-guile-debug--send-to-repl 'locals))

(defun geiser-guile-debug-show-registers ()
  "Show register values."
  (interactive)
  (geiser-guile-debug--send-to-repl 'registers))

(defun geiser-guile-debug-show-error ()
  "Show error message."
  (interactive)
  (geiser-guile-debug--send-to-repl 'error))

(transient-define-prefix geiser-guile--debug-transient ()
  "Debugging meta-commands."
  ["Guile debugger"
   [("n" "Next error" compilation-next-error)
    ("p" "Previous error" compilation-next-error)
    ("z" "Scheme buffer" geiser-debug-switch-to-buffer)
    ("x" "Exit debug level" geiser-guile-debug-quit)]
   [("b" "Show backtrace" geiser-guile-debug-show-backtrace)
    ("e" "Show error" geiser-guile-debug-show-error)
    ("l" "Show locals" geiser-guile-debug-show-locals)
    ("r" "Show registers" geiser-guile-debug-show-registers)]])

(defun geiser-guile-debug-menu ()
  "Show available debugging commands, if any."
  (interactive)
  (when (and (eq 'guile geiser-impl--implementation) (geiser-debug-active-p))
    (call-interactively #'geiser-guile--debug-transient)))

(define-key geiser-debug-mode-map "," #'geiser-guile-debug-menu)

(defun geiser-guile--enter-debugger ()
  "Tell Geiser to interact with the debugger."
  (when geiser-guile-show-debug-help
    (message "Debugger active. Press , for commands."))
  nil)

(defun geiser-guile--display-error (_module _key msg)
  "Display error with given message MSG."
  (when (stringp msg)
    (geiser-guile--set-up-error-links)
    (save-excursion (insert msg)))
  (not (zerop (length msg))))


;;; Trying to ascertain whether a buffer is Guile Scheme

(defconst geiser-guile--guess-re
  (format "\\(%s\\|#! *.+\\(/\\| \\)guile\\( *\\\\\\)?\\)"
          geiser-guile--module-re))

(defun geiser-guile--guess ()
  "Ascertain whether we are in a Guile file."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward geiser-guile--guess-re nil t)))


;;; Keywords and syntax

(defconst geiser-guile--builtin-keywords
  '("call-with-input-file"
    "call-with-input-string"
    "call-with-output-file"
    "call-with-output-string"
    "call-with-prompt"
    "call-with-trace"
    "define-accessor"
    "define-class"
    "define-enumeration"
    "define-inlinable"
    "define-syntax-parameter"
    "eval-when"
    "lambda*"
    "syntax-parameterize"
    "use-modules"
    "with-error-to-file"
    "with-error-to-port"
    "with-error-to-string"
    "with-fluid*"
    "with-fluids"
    "with-fluids*"
    "with-input-from-port"
    "with-input-from-string"
    "with-output-to-port"
    "with-output-to-string"))

(defun geiser-guile--keywords ()
  "Return Guile-specific scheme keywords."
  (append
   (geiser-syntax--simple-keywords geiser-guile-extra-keywords)
   (geiser-syntax--simple-keywords geiser-guile--builtin-keywords)
   `((,(rx "(" (group "define-once") eow (* space) (? (group (+ word))))
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
     ("(\\(define-module\\) +(\\([^)]+\\))"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face nil t)))))

(geiser-syntax--scheme-indent
 (c-declare 0)
 (c-lambda 2)
 (call-with-input-string 1)
 (call-with-output-string 0)
 (call-with-prompt 1)
 (call-with-trace 0)
 (eval-when 1)
 (lambda* 1)
 (pmatch defun)
 (sigaction 1)
 (syntax-parameterize 1)
 (with-error-to-file 1)
 (with-error-to-port 1)
 (with-error-to-string 0)
 (with-fluid* 1)
 (with-fluids 1)
 (with-fluids* 1)
 (with-input-from-string 1)
 (with-method 1)
 (with-mutex 1)
 (with-output-to-string 0)
 (with-throw-handler 1))


;;; REPL startup

(defconst geiser-guile-minimum-version "2.2")

(defun geiser-guile--version (_binary)
  "Find Guile's version running the configured Guile binary."
  ;; maybe one day we'll have `process-lines' with tramp support
  (let* ((unixy (not (member system-type '(windows-nt ms-dos cygwin))))
         (shell-command-switch (if unixy "-c" shell-command-switch))
         (shell-file-name (if unixy "sh" shell-file-name)))
    (shell-command-to-string
     (format "%s -c %s 2>/dev/null"
             (geiser-guile--binary)
             (shell-quote-argument "(display (version))")))))

(defun geiser-guile-update-warning-level ()
  "Update the warning level used by the REPL.
The new level is set using the value of `geiser-guile-warning-level'."
  (interactive)
  (let ((code `(:eval (:ge set-warnings ',geiser-guile-warning-level)
                      (geiser evaluation))))
    (geiser-eval--send/result code)))

(defun geiser-guile-update-doc-process-texinfo ()
  "Update whether docstrings should be processed as texinfo.
The new value is set using the value of `geiser-guile-doc-process-texinfo'."
  (interactive)
  (let* ((new-value (if geiser-guile-doc-process-texinfo
                        '\#t
                      '\#f))
         (code `(begin (set! (@@ (geiser doc) %process-texinfo?) ,new-value)
                       'done)))
    (geiser-eval--send/wait code)))

;;;###autoload
(defun connect-to-guile ()
  "Start a Guile REPL connected to a remote process.

Start the external Guile process with the flag --listen to make
it spawn a server thread."
  (interactive)
  (geiser-connect 'guile))

(defun geiser-guile--set-geiser-load-path ()
  "Set up scheme load path for REPL."
  (let* ((path (geiser-guile-ensure-scheme-dir))
         (witness "geiser/emacs.scm")
         (code `(begin (if (not (%search-load-path ,witness))
                           (set! %load-path (cons ,path %load-path)))
                       'done)))
    (geiser-eval--send/wait code)))

(defun geiser-guile--set-up-declarative-modules ()
  "Set up Guile to (not) use declarative modules.
See `geiser-guile-use-declarative-modules'."
  (unless geiser-guile-use-declarative-modules
    (let ((code '(begin (eval-when (expand) (user-modules-declarative? :f)) 'ok)))
      (geiser-eval--send/wait code))))

(defun geiser-guile--set-up-backtrace ()
  "Set up Guile's backtrace properties."
  (when geiser-guile-debug-backwards-backtrace
    (geiser-eval--send/wait '(debug-enable 'backwards)))
  (when (> geiser-guile-debug-terminal-width 0)
    (geiser-eval--send/wait `(begin ((@ (system repl debug) terminal-width)
                                     ,geiser-guile-debug-terminal-width)
                                    'ok))))

(defun geiser-guile--startup (remote)
  "Startup function, for a remote connection if REMOTE is t."
  (geiser-guile--set-up-error-links)
  (let* ((last-scm (or geiser-repl--last-scm-buffer (current-buffer)))
         (geiser-log-verbose t)
         (g-load-path (buffer-local-value 'geiser-guile-load-path last-scm)))
    (when (or geiser-guile--conn-address remote)
      (geiser-guile--set-geiser-load-path))
    (geiser-guile--set-up-declarative-modules)
    (geiser-guile--set-up-backtrace)
    (geiser-eval--send/wait ",use (geiser emacs)\n'done")
    (dolist (dir g-load-path)
      (let ((dir (expand-file-name dir)))
        (geiser-eval--send/wait `(:eval (:ge add-to-load-path ,dir)))))
    (let ((geiser-guile-warning-level
           (buffer-local-value 'geiser-guile-warning-level last-scm)))
      (geiser-guile-update-warning-level))
    (let ((geiser-guile-doc-process-texinfo
           (buffer-local-value 'geiser-guile-doc-process-texinfo last-scm)))
      (geiser-guile-update-doc-process-texinfo))))


;;; Manual lookup

(defun geiser-guile--info-lookup (id)
  (cond ((null id) (info "guile"))
        ((ignore-errors (info-lookup-symbol (format "%s" id) 'geiser-guile-mode) t))
        ((and (listp id) (geiser-guile--info-lookup (car (last id)))))
        (t (geiser-guile--info-lookup (when (listp id) (butlast id))))))

(defun geiser-guile--manual-look-up (id _mod)
  "Look for ID in the Guile manuals."
  (let ((info-lookup-other-window-flag geiser-guile-manual-lookup-other-window))
    (geiser-guile--info-lookup id)
    (when geiser-guile-manual-lookup-other-window
      (switch-to-buffer-other-window "*info*"))))


;;; Implementation definition:

(define-geiser-implementation guile
  (binary geiser-guile--binary)
  (arglist geiser-guile--parameters)
  (version-command geiser-guile--version)
  (minimum-version geiser-guile-minimum-version)
  (repl-startup geiser-guile--startup)
  (prompt-regexp geiser-guile--prompt-regexp)
  (clean-up-output geiser-guile--clean-up-output)
  (debugger-prompt-regexp geiser-guile--debugger-prompt-regexp)
  (enter-debugger geiser-guile--enter-debugger)
  (marshall-procedure geiser-guile--geiser-procedure)
  (find-module geiser-guile--get-module)
  (enter-command geiser-guile--enter-command)
  (exit-command geiser-guile--exit-command)
  (import-command geiser-guile--import-command)
  (find-symbol-begin geiser-guile--symbol-begin)
  (display-error geiser-guile--display-error)
  (external-help geiser-guile--manual-look-up)
  (check-buffer geiser-guile--guess)
  (keywords geiser-guile--keywords)
  (case-sensitive geiser-guile-case-sensitive))

;;;###autoload
(geiser-activate-implementation 'guile)

;;;###autoload
(autoload 'run-guile "geiser-guile" "Start a Geiser Guile REPL." t)

;;;###autoload
(autoload 'switch-to-guile "geiser-guile"
  "Start a Geiser Guile REPL, or switch to a running one." t)

(provide 'geiser-guile)
;;; geiser-guile.el ends here
