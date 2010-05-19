;;; edict-edit.el --- Edit an EDICT dictionary.

;; Copyright (C) 1991, 1992 Per Hammarlund (perham@nada.kth.se)

;; Author:      Per Hammarlund <perham@nada.kth.se>
;; Keywords:    mule, edict, dictionary
;; Version:     0.9.8
;; Adapted-by:  Stephen J. Turnbull <turnbull@sk.tsukuba.ac.jp> for XEmacs
;; Maintainer:  Stephen J. Turnbull <turnbull@sk.tsukuba.ac.jp>

;;   This file is part of XEmacs.

;;   XEmacs is free software; you can redistribute it and/or modify it
;;   under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2, or (at your
;;   option) any later version.

;;   XEmacs is distributed in the hope that it will be useful, but
;;   WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;   General Public License for more details.
;; 
;;   You should have received a copy of the GNU General Public License
;;   along with XEmacs; if not, write to the Free Software Foundation,
;;   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Some code that looks for translations of english and japanese using the
;; EDICTJ Public Domain japanese/english dictionary.

;; Written by Per Hammarlund <perham@nada.kth.se>
;; Morphology and private dictionary handling/editing by Bob Kerns
;; <rwk@crl.dec.com>
;; Helpful remarks from Ken-Ichi Handa <handa@etl.go.jp>.
;; The EDICTJ PD dictionary is maintained by Jim Breen
;; <jwb@monu6.cc.monash.edu.au>

;;; To do:

;;; Changelog:

;;; Code:

(require 'cl)

;;; Customizable variables

(defvar edict-use-electric-henkan nil
  "Determines whether to use electric henkan mode in edict buffers.

If t, use it; if nil, don't use it.  If 'ask, ask and (re)set the flag.")

(defvar edict-verbose-electric-henkan t
  "If non-nil, warns the user of electric changes in henkan state.")

;;; Internal variables

;; The previous configuration before adding an entry to a private dictionary.
(defvar edict-previous-window-configuration nil)

;; The previously-selected buffer before adding an entry.
(defvar edict-previous-buffer nil)

;; The filename of the file read in to add an entry to.
(defvar edict-filename nil)

(defvar edict-edit-mode-map nil
  "Mode map used by edict-add-english/kanji.")

;; Initialize our mode map.
(unless edict-edit-mode-map
  (setq edict-edit-mode-map (make-keymap))
  (if (featurep 'xemacs)
      (map-keymap (lambda (key)
		    (define-key edict-edit-mode-map key 'edict-standin))
		  edict-edit-mode-map)
    (dotimes (i 128)
      ;; #### I hope this is OK without the check below
      (define-key edict-edit-mode-map [ i ] 'edict-standin)))
; Emacs 18?
;      ;; I don't know how to invoke multi-char commands, so don't hook
;      ;; those.
;      (unless (consp (aref edict-edit-mode-map i))
;	(setf (aref edict-edit-mode-map i) 'edict-standin))))
  (if (featurep 'xemacs)
      (progn
	(define-key edict-edit-mode-map [(control c)] nil)
	(define-key edict-edit-mode-map [(control x)] nil)
	(define-key edict-edit-mode-map [(escape)] nil))
    (define-key edict-edit-mode-map [ 3 ] nil)
    (define-key edict-edit-mode-map [ 24 ] nil)
    (define-key edict-edit-mode-map [ 27 ] nil))
; Emacs 18?
;    (setf (aref edict-edit-mode-map 3) nil
;	  (aref edict-edit-mode-map 24) nil
;	  (aref edict-edit-mode-map 27) nil))
  (define-key edict-edit-mode-map "\C-c\C-c" 'edict-exit)
  (define-key edict-edit-mode-map "\C-x\C-s" 'edict-exit)
  (define-key edict-edit-mode-map "\t" 'edict-tab)
  (define-key edict-edit-mode-map "\r" 'edict-new-entry)
  (define-key edict-edit-mode-map "\C-A" 'edict-beginning-of-line)
  (define-key edict-edit-mode-map "\C-E" 'edict-end-of-line)
  (define-key edict-edit-mode-map "[" 'edict-open-bracket)
  (define-key edict-edit-mode-map "]" 'edict-close-bracket)
  (define-key edict-edit-mode-map "/" 'edict-slash))

;;; Functions

;; Add an entry to a particular file, and update edict-buffer.
;; Any of kanji/yomi/eigo may be omitted.  The user will be given
;; an oportunity to edit and then it will be saved.

;; #### This isn't interactive, but it's not an unreasonable entry point?
(defun edict-add-entry-to-file (filename kanji yomi eigo)
  (edict-init)
  (setq filename (expand-file-name filename))
  (let* ((previous-buffer (current-buffer))
	 (buffer (find-file-noselect filename))
	 (window (get-buffer-window buffer)))
    (set-buffer buffer)
    ;; If it's a new file, give it a version string to print on loadup.
    (when (equal (point-min) (point-max))
      (insert (format "？？？？ /%s's private dictionary/\n"
		      (user-login-name))))
    ;;	Unless it's already in edict-edit mode, put it in that mode.
    ;; This gives us our fancy electric-dictionary editing.
    (unless (eq major-mode 'edict-edit-mode)
      (edict-edit-mode))
    ;; Unless we already have a configuration to go back to, remember
    ;; this one.
    (unless edict-previous-window-configuration
      (setq edict-previous-window-configuration
	    (current-window-configuration)))
    (unless edict-previous-buffer
      (setq edict-previous-buffer previous-buffer))
    ;; Remember the filename, so we can update it in the *edict* buffer
    ;; when we finish.
    (setq edict-filename filename)
    (if window
	(select-window window)
      (split-window nil 4))
    (goto-char (point-max))
    (edict-insert-entry kanji yomi eigo)
    ;; Go into henkan mode if appropriate
    (switch-to-buffer buffer)
    (edict-set-henkan (or (null kanji) (null yomi)))))


;; Turn on or off henkan
;; Should work in any Mule environment, in particular, not require LEIM.
;; #### Probably fails pretty impolitely if no Japanese input methods are
;;	registered with Mule.
;; The guts were copied from mule-commands.el (toggle-input-method).
;;
(defun edict-set-henkan (henkan-flag)
  "Electrically turn on or off the current default Japanese text input method.

If HENKAN-FLAG is nil, turn it off, otherwise turn it on.
With arg, read an input method from minibuffer and turn it on."

  (if (eq 'ask edict-use-electric-henkan)
      (if (and (featurep 'xim)
	       (y-or-n-p
		"XIM and electric-henkan don't mix.  Disable electric-henkan"))
	  (setq edict-use-electric-henkan nil)
	(setq edict-use-electric-henkan t))
    (setq edict-use-electric-henkan t))
  (if edict-use-electric-henkan
      (let* ((default (or (car input-method-history) default-input-method)))
	(if (and current-input-method (not henkan-flag))
	    (inactivate-input-method)
	  ;; #### Need to ensure that the IM is Japanese.  Could do
	  ;;	     by looking up in registry, and requiring confirmation
	  ;;	     if some heuristic isn't satisfied. 
	  (activate-input-method
	   (if (or henkan-flag (not default))
	       (read-input-method-name
		(if default
		    "Japanese input method (default %s): "
		  "Japanese input method: " )
		default t)  
	     default))
	  (or default-input-method
	      (setq default-input-method current-input-method)))
	(and edict-verbose-electric-henkan
	     (message "Henkan is electrically %s."
		      (if henkan-flag "on" "off"))))))

;; Insert a dictionary entry at point.
(defun edict-insert-entry (kanji yomi eigo)
  ;; Make sure this is on a line of its own.
  (let ((p (point)))
    (beginning-of-line)
    (unless (equal p (point))
      (end-of-line)
      (newline)))
  ;; Now insert a standard entry.
  (let ((start (point))
	(p nil))
    ;; Insert a new entry, leaving out any items which are nil,
    ;; and also leaving out the yomi if the entry consists of only kana.
    ;; "日本語"
    (if kanji
	(insert kanji)
      (setq p (point)))
    (when yomi
      (unless (string-match edict-yomi-regexp yomi)
	(error "yomi must be in kana: %s." yomi)))
    ;; "日本語 [にほんご]"
    (cond ((and kanji
		(string-match edict-yomi-regexp kanji)))
	  (t (insert " [")
	     (if yomi
		 (insert yomi)
	       (if (not p)
		   (setq p (point))))
	     (insert "]")))
    ;; "日本語 [にほんご] /Japanese language/"
    (cond ((null eigo)
	   (insert " /")
	   (unless p (setq p (point))))
	  ((stringp eigo)
	   (insert " /" eigo))
	  ((consp eigo)
	   (insert " ")
	   (dolist (def eigo)
	     (insert "/")
	     (insert def)))
	  (t (error "not a string or list of strings: %s" eigo)))
    (insert "/\n")
    ;; Go to the first un-filled-in field.
    (goto-char (or p start))))

;; Inverse of edict-insert-entry.  Parse an entry.
;; (multiple-value-bind (kanji yomi english) (edict-parse-entry)
;;    (edict-insert-entry kanji yomi english))
;; duplicates the current line's entry.

(defun edict-parse-entry ()
  (let ((kanji nil)
	(yomi nil)
	(english nil)
	(start nil)
	(p nil)
	(end nil))
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq start (point))
      (search-forward " " end)
      (setq p (1- (point)))
      (when (> p start)
	(setq kanji (buffer-substring start p)))
      ;; Pick up the [yomi] if there are any.
      (when (re-search-forward edict-yomi-part-regexp end t)
	(setq yomi (buffer-substring (match-beginning 1) (match-end 1)))
	(goto-char (match-end 0)))
      ;; Collect up all the definitions.
      (while (re-search-forward "/\\([^/\n]+\\)/" end t)
	(goto-char (match-end 1))
	(push (buffer-substring (match-beginning 1) (match-end 1)) english)))
    (values kanji yomi english)))

;;;###autoload
(defun edict-edit-mode ()
  "Major mode for editing edict entries.
TAB      Tab to next field in this entry.
RETURN   Start a new entry on the next line.
c-A      Edit the kanji field, and start entering kanji.
c-E      Go to the end, and start editing english.
C-c C-c  Install the edited changes & save the file.
C-x C-s  Install the edited changes & save the file.
"
  (interactive)
  (kill-all-local-variables)
  ;; Associate these with the buffer.
  (make-local-variable 'edict-previous-window-configuration)
  (make-local-variable 'edict-previous-bufffer)
  (make-local-variable 'edict-filename)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map edict-edit-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'edict-edit-mode)
  (setq mode-name "Edict")
  (setq paragraph-start "^\\|$")
  (setq paragraph-separate "^\\|$")
  (run-hooks 'text-mode-hook))

;; Automagically pick the right mode, based on where we are in the string.
;; That's henkan mode when we're in the entry or yomi sections, and english
;; in the translation section.
;; #### Can this be better done with extents or overlays?
(defun edict-auto-set-henkan ()
  (save-excursion
    (let ((x (point))
	  (end nil))
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (edict-set-henkan
       (or (looking-at "$")
	   (when (re-search-forward "[]/]" end t)
	     (<= x (match-beginning 0))))))))

(defun edict-standin ()
  "Invoke the command we would otherwise have invoked, after being sure
we're in the right mode."
  (interactive)
  ;; #### This is evil, I think.
  (setq this-command (aref global-map last-command-char))
  (edict-execute-dictionary-command
   (function (lambda ()
	       (command-execute this-command)))))

(defun edict-execute-dictionary-command (function)
  (edict-auto-set-henkan)
  (let ((buffer (current-buffer)))
    ;; Canonicalize the end to end in exactly one slash.
    (unless (<= (point) (point-min))
      (save-excursion
	(backward-char 1)
	(when (looking-at "//\n")
	  (forward-char 1)
	  (delete-char 1))))
    (funcall function)
    ;; Canonicalize the end of the line to end in exactly one slash.
    (save-excursion
      (end-of-line)
      (delete-horizontal-space)
      (unless (<= (point) (point-min))
	(backward-char 2)
	(while (looking-at "//")
	  ;; Two in a row; delete the second.
	  (forward-char 1)
	  (delete-char 1)
	  (backward-char 2))
	(forward-char 1)
	(unless (looking-at "\n")
	  (unless (looking-at "[/\n]")
	    (end-of-line)
	    (unless (edict-line-has-english)
	      (insert " /"))
	    (insert ?/)))))
    ;; Then if we are at the end, make it end in two, for the sake of visual feedback.
    ;; Except if we're on a blank line, don't add anything.
    (unless (<= (point) (point-min))
      (unless (save-excursion
		(end-of-line)
		(backward-char 1)
		(looking-at "\n"))
	(when (looking-at "\n")
	  (insert "/")
	  (backward-char 1))
	(save-excursion
	  (end-of-line)
	  ;; Make sure there's a trailing newline.
	  (when (>= (point) (point-max))
	    (newline)
	    (backward-char 1))
	  (let ((end (point)))
	    (beginning-of-line)
	    (when (search-forward "/" end t)
	      (when (looking-at "\n")
		(insert "/")))))))
    ;; Only set the henkan if we're still in the same buffer.
    (when (eq buffer (current-buffer))
      (edict-auto-set-henkan))))

(defun edict-line-has-english (&optional complete)
  (save-excursion
    (let ((p (point)))
      (end-of-line)
      (let ((end (point)))
	(goto-char p)
	(beginning-of-line)
	(if complete
	    (re-search-forward "/[^/\n]+/" end t)
	  (re-search-forward "/" end t))))))

(defvar *brackets-allowed-in-english* nil
  "*Allow brackets in the english section of dictionary entries, if non-null.")

(defun edict-open-bracket ()
  "Begin editing the yomi section of the entry, at the beginning of the entry.
Self-inserts if in the english section."
  (interactive)
  (edict-execute-dictionary-command (function (lambda ()
						(edict-char-bracket t)))))

(defun edict-close-bracket ()
  "Begin editing the yomi section of the entry, at the end of the entry.
Self-inserts if in the english section.."
  (interactive)
  (edict-execute-dictionary-command (function (lambda ()
						(if (looking-at "\\]")
						    (edict-tab)
						  (edict-char-bracket nil))))))

(defun edict-char-bracket (open-p)
  (let ((p (point)))
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (cond ((and *brackets-allowed-in-english*
		  (save-excursion
		    (re-search-forward "/[^\n/]*/" end t))
		  (<= (match-beginning 0) p))
	     (goto-char p)
	     (edict-standin))
	    ((re-search-forward edict-yomi-part-regexp end t)
	     (goto-char (or (if open-p
				(match-beginning 1)
			      (match-end 1))
			    ;; Empty
			    (1+ (match-beginning 0)))))
	    ((re-search-forward "[ \t]" end t)
	     (goto-char (match-beginning 0))
	     (insert " []")
	     (backward-char 1))
	    (t (goto-char p)
	       (edict-standin))))))

(defun edict-slash ()
  "Begin editing the english section of the entry, at the start of the entry.
Self-inserts if in the english section."
  (interactive)
  (edict-execute-dictionary-command (function edict-slash-internal)))

(defun edict-slash-internal ()
  (if (looking-at "/\n")
      (forward-char)
    (let ((p (point)))
      (end-of-line)
      (let ((end (point)))
	(beginning-of-line)
	(cond ((and (save-excursion
		      (re-search-forward "/[^/\n]*/" end t))
		    (<= (match-beginning 0) p))
	       (goto-char p)
	       (edict-standin))
	      ((search-forward "/" end t))
	      ;; On an empty line, just insert a definition.
	      ((looking-at "$")
	       (insert " //")
	       (backward-char 1))
	      ;; Otherwise, this line has no english, go to the end and add one.
	      (t (end-of-line)
		 (backward-char 1)
		 (unless (looking-at " ")
		   (insert " "))
		 (insert "//")
		 (backward-char 1)))))))

(defun edict-tab ()
  "Tab to the next edict field in this entry.
At the end, wraps back to the beginning.."
  (interactive)
  (edict-execute-dictionary-command (function edict-tab-internal)))

(defun edict-tab-internal ()
  (let ((p (point))
	(end nil))
    (end-of-line)
    (setq end (point))
    (goto-char p)
    (cond ((re-search-forward "[ \t]\\(\\[\\)\\|\\(/\\)" end t)
	   (let ((f-begin (or (match-beginning 1) (match-beginning 2)))
		 (f-end (or (match-end 1) (match-end 2))))
	     (goto-char f-begin)
	     (edict-set-henkan (looking-at "\\["))
	     (goto-char f-end)))
	  (t (beginning-of-line)
	     (edict-set-henkan t)))))

(defun edict-beginning-of-line ()
  "Go to the beginning of the edict entry."
  (interactive)
  (edict-execute-dictionary-command (function (lambda ()
						(beginning-of-line)
						(edict-set-henkan t)))))

(defun edict-end-of-line ()
  "Go to the beginning of the edict entry."
  (interactive)
  (edict-execute-dictionary-command (function (lambda ()
						(end-of-line)
						(edict-set-henkan nil)))))

(defun edict-new-entry (arg)
  "Start a new edict entry on the next line.
If given an argument, copies the word but not the yomi or english.
If given an argument > 4 (i.e. c-U c-U), copies the word and definition,
but not the yomi."
  (interactive "P")
  (edict-execute-dictionary-command (function (lambda ()
						(edict-new-entry-internal arg)))))

(defun edict-new-entry-internal (arg)
  (end-of-line)
  ;;clean up in the dictionary to save space.
  (delete-horizontal-space)
  ;;first check that the last thing on this line is a '/', otherwise add one.
  (unless (<= (point) (point-min))
    (backward-char)
    (unless (looking-at "/")
      (end-of-line)
      (insert "/"))
    (multiple-value-bind (kanji yomi english)
	(edict-parse-entry)
      (end-of-line)
      (if (>= (point) (point-max))
	  (newline)
	(forward-char 1))
      (cond ((null arg)
	     (edict-insert-entry nil nil nil))
	    ((<= (prefix-numeric-value arg) 4)
	     (edict-insert-entry kanji nil nil))
	    (t (edict-insert-entry kanji nil english))))))

(defun edict-exit ()
  "Exit the editing of a private edict file, saving the buffer and updating the
running copy of the dictionary, and restoring the window configuration."
  (interactive)
  (save-buffer)
  (let* ((buffer (current-buffer))
	 (edict-private-buffer (find-file-noselect (expand-file-name edict-user-dictionary)))
	 (filename (or edict-filename (buffer-file-name edict-private-buffer)))
	 (configuration edict-previous-window-configuration)
	 (previous-buffer edict-previous-buffer))
    (setq edict-previous-window-configuration nil
	  edict-previous-buffer nil)
    (set-buffer edict-buffer)
    (goto-char (point-min))
    (let ((begin-marker (format "%s %s" *edict-file-begin-marker* filename))
	  (end-marker (format "%s %s" *edict-file-end-marker* filename)))
      (if (search-forward begin-marker nil t)
	  (progn
	    (forward-line 1)
	    (let ((loc (point)))
	      (search-forward end-marker)
	      (forward-line 0)
	      (delete-region loc (point))
	      (goto-char loc)))
	;; Handle new file
	(insert (format "%s\n%s\n" begin-marker end-marker))
	(forward-line -1))
      (insert-buffer buffer)
      (when configuration
	(set-window-configuration configuration))
      (when previous-buffer
	(switch-to-buffer previous-buffer)))))

;;;###autoload
(defun edict-add-word ()
  "Add any word to the private dictionary."
  (interactive)
  (edict-add-entry-to-file edict-user-dictionary nil nil nil))

;;;###autoload
(defun edict-add-english ()
  "Add the english word at point to the dictionary."
  (interactive)
  (let ((word (edict-get-english-word)))
    (when word
      (edict-add-entry-to-file edict-user-dictionary nil nil word))))

;;;###autoload
(defun edict-add-kanji (min max)
  "Add the region as a kanji entry in the dictionary."
  (interactive "r")
  (edict-add-entry-to-file edict-user-dictionary
			   (edict-clean-up-kanji (buffer-substring min max))
			   nil nil))

(provide 'edict-edit)

;;; edict-edit.el ends here
