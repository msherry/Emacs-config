;;; arduino-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from arduino-mode.el

(autoload 'arduino-sketch-new "arduino-mode" "\
A command to create new `SKETCH' in ARDUINO_HOME (~/Arduino).

(fn SKETCH)" t)
(autoload 'arduino-mode "arduino-mode" "\
Major mode for editing Arduino code.

(fn)" t)
(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
(register-definition-prefixes "arduino-mode" '("arduino-"))


;;; Generated autoloads from ede-arduino.el

(defvar ede-arduino-preferences-file "~/.arduino/preferences.txt" "\
The location of personl preferences for the arduino IDE.
Note: If this changes, we need to also update the autoload feature.")
(custom-autoload 'ede-arduino-preferences-file "ede-arduino" t)
(eieio-defclass-autoload 'ede-arduino-prefs 'nil "\
ede-arduino" "Class containing arduino preferences.")
(eieio-defclass-autoload 'ede-arduino-board 'nil "\
ede-arduino" "Class for containing key aspect of the arduino board.")
(autoload 'ede-arduino-root "ede-arduino" "\
Get the root project directory for DIR.
The only arduino sketches allowed are those configured by the arduino IDE
in their sketch directory.

If BASEFILE is non-nil, then convert root to the project basename also.

Consider expanding this at some later date.

(fn &optional DIR BASEFILE)")
(autoload 'ede-arduino-file "ede-arduino" "\
Get a file representing the root of this arduino project.
It is a file ending in .pde or .ino that has the same basename as
the directory it is in.  Optional argument DIR is the directory
to check.

(fn &optional DIR)")
(autoload 'ede-arduino-load "ede-arduino" "\
Return an Arduino project object if there is one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is not used, sinc there is only one project for a directory tree.

(fn DIR &optional ROOTPROJ)")
(require 'ede/auto)
(add-to-list 'ede-project-class-files (ede-project-autoload :name "Arduino sketch" :file 'ede-arduino :proj-root-dirmatch (ede-project-autoload-dirmatch :fromconfig (expand-file-name ede-arduino-preferences-file) :configregex "^sketchbook.path=\\([^
]+\\)$" :configregexidx 1) :proj-file 'ede-arduino-file :proj-root 'ede-arduino-root :load-type 'ede-arduino-load :class-sym 'ede-arduino-project :safe-p t :new-p t) t)
(eieio-defclass-autoload 'ede-arduino-target '(ede-target) "\
ede-arduino" "EDE Arduino C files target.  Includes PDE, C, C++ and anything else we find.")
(eieio-defclass-autoload 'ede-arduino-project '(ede-project) "\
ede-arduino" "EDE Arduino project.")
(register-definition-prefixes "ede-arduino" '("cedet-arduino-serial-monitor" "ede-arduino"))


;;; Generated autoloads from flycheck-arduino.el

(autoload 'flycheck-arduino-setup "flycheck-arduino" "\
Setup Flycheck Arduino.
Add `arduino' to `flycheck-checkers'." t)
(register-definition-prefixes "flycheck-arduino" '("flycheck-arduino-board"))


;;; Generated autoloads from ob-arduino.el

(autoload 'org-babel-execute:arduino "ob-arduino" "\
org-babel arduino hook.

(fn BODY PARAMS)")
(with-eval-after-load 'org (add-to-list 'org-src-lang-modes '("arduino" . arduino)) (add-to-list 'org-babel-tangle-lang-exts '("arduino" . "ino")))
(register-definition-prefixes "ob-arduino" '("ob-arduino:" "org-babel-default-header-args:sclang"))

;;; End of scraped data

(provide 'arduino-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; arduino-mode-autoloads.el ends here
