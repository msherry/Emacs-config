;;; dui.el --- Dictionary user interface

;; Copyright (C) 1998 by Stephen J. Turnbull

;; Author:      Stephen J. Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Keywords:    mule, dictionary
;; Version:     0.6

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

;; Some code that provides support for dictionary lookup and manipulations
;; (such as inserting definitions and maintaining a private dictionary).
;; Originally written in support of edict.el, by Per Hammarlund
;; <perham@nada.kth.se>, but since generalized.

;;; To do:

;;; Changelog:

;; 1998-03-27  Stephen Turnbull  <turnbull@sk.tsukuba.ac.jp>
;;        (created):  broken out from monolithic edict.el

;;; Code:

;;; Dictionary lookup minor mode (dl-mode)

;; User customization variables

(defvar dl-mode-prefix '[(control ?c) (?\$)]
  "Prefix key sequence for dl-mode command keys.

After loading, change the mode's prefix by using dl-mode-set-prefix;
setq'ing this variable can't work.")

(defvar dl-indicator-string " dl"
  "String indicating activation of dl minor mode in the modeline.

Set to nil to inhibit modeline display.")

;; A convention for modes; here honored by observance, not breach.
;;
(defvar dl-mode-hook nil
  "A normal hook called at the end of the dl-mode activation process.

If you can think of a use for this, you're more clever than I.")

;; Auxiliary customizations

(defvar dl-conflict-warning "Binding conflict: %s -> %s."
  "Format string warning about key sequences with conflicting bindings.

Must contain two `%s' descriptors.  The first formats the key sequence,
the second the description of the existing binding.")

(defvar dl-warn-conflict-verbosity 3
  "Controls verbosity of binding conflict warnings.

0   turns off warnings entirely.
1   issues a warning for each binding conflict (including sub-keymaps).
2   prints a summary message with a count of conflicts (not including
    sub-keymaps, only keys in those maps that have conflicts).
3   adds verbose detail about what is being done.

Each positive level performs all actions of lower levels.")

;; The basic mode conventions.

;; Mode flag and keymaps
;;
(defvar dl-mode nil "Activation flag for dl-mode.")

(defvar dl-mode-submap nil
  "Sub-keymap for dl-mode.

All key sequences are prefixed by the sequence defined in dl-mode-map.")

(defvar dl-mode-map nil
  "Keymap for dl-mode.  Holds the prefix key for dl-mode functions.

Its value will be installed into minor-mode-map-alist.  Prefix cannot
be altered by setq'ing dl-mode-map.  Use dl-mode-set-prefix instead.")

;; Mode toggle
;;
;; The side effect is arguably not a feature :-)
;
;;;###autoload
(defun dl-mode (&optional arg)
  "Minor mode for dictionary lookup, with interfaces to dictionary utilities.

Null ARG toggles the mode, positive ARG turns the mode on, negative ARG
turns the mode off.

When the mode is already off, (dl-mode -1) has the side effect of checking
and reporting any conflicting bindings.

\\{dl-mode-map}"

  (interactive "P")

  ;; dl-warn-binding-conflicts doesn't make sense when the mode is active
  (if (null dl-mode)
      (dl-warn-binding-conflicts dl-mode-map))
  (setq dl-mode (if (null arg) (not dl-mode)
                  (> (prefix-numeric-value arg) 0)))
  (run-hooks dl-mode-hook))

;; Internal mode data

;; Main keymap
;;
(or dl-mode-submap
    (progn
     (define-prefix-command 'dl-mode-submap) ;
     (define-key dl-mode-submap '[ ?s ] 'dui-invoke-search-method)
     (define-key dl-mode-submap '[ ?i ] 'dui-invoke-insert-method)
     ;; Hmm ... I don't think there are any of these :-P
     ;;(define-key dl-mode-submap '[ ?e ] 'dui-invoke-edit-method)
     (define-key dl-mode-submap '[ ?d ] 'dui-describe-method)
     ))

;; Helper functions

;; Set the mode prefix
;;
;; This can't be done simply by setq'ing dl-mode-map; minor-mode-map-alist
;; does not refer to that variable but contains a copy of its value.
;;
(defun dl-mode-set-prefix (key &optional context)
  "Set the prefix key sequence for dl-mode to KEY.

Return the new dl-mode-map.  When called interactively read KEY from
the minibuffer (as a string; keys not bound to `self-insert' must be
quoted with C-q).  If you need more flexibility than ASCII gives, you'll
have to use the `eval-expression' interface.

Example: `\\[dl-mode-set-prefix] C-q C-c $ RET' returns the prefix key
to the default `C-c $'.

Allowed values of CONTEXT:
NIL                substitute a map containing KEY in minor-mode-map-alist.
adding-minor-mode  manipulation of minor-mode-map-alist is done elsewhere."

  ;; Should read key events but I don't know how to make that work.
  (interactive "Key sequence (quote control characters with ^Q): ")

  (setq dl-mode-map (make-sparse-keymap))
  (define-key dl-mode-map key 'dl-mode-submap)
  (cond ((null context)
	 (let ((slot (assq 'dl-mode minor-mode-map-alist)))
	   (setq minor-mode-map-alist
		 (cons (cons 'dl-mode dl-mode-map)
		       (if slot
			   (delete slot minor-mode-map-alist)
			 minor-mode-map-alist)))))
	((equal context 'adding-minor-mode))
	(t (error "Illegal context `%s' in dl-mode-set-prefix." context)))
  dl-mode-map)

;; Deal with binding conflicts
;;
;; Search keymaps for binding conflicts for each key in the mode's keymap.
;; Current implementation searches only active keymaps; it won't tell
;; about inactive keymaps, including those of minor modes that might be
;; invoked later or (worse) major modes already invoked in other buffers.
;;
(defun dl-warn-binding-conflicts (map)
  "Warn about key bindings that will conflict with those in MAP.

Results will be non-sensical if MAP is invoked via a prefix or is
already active.  The current implementation only looks in the active
keymaps.  Maps of inactive minor modes and local maps major modes of
other buffers will not be searched (although the latter will be shadowed
since dl-mode is a global variable)."
  (if (null (featurep 'xemacs))
      ;; `map-keymap' doesn't exist in the FSF's Emacs
      (message "Keymap shadow checking not supported under\n%s"
	       (emacs-version))
    (let ((found 0))
      (if (> dl-warn-conflict-verbosity 1)
	  (progn
	    (message "Checking for conflicting bindings...")
	    (if (> dl-warn-conflict-verbosity 2)
		(message "Examining accessible maps of map:\n    `%s'" map))))
      ;; A map is accessible from itself
      (mapcar (lambda (slot)
		(let ((prefix (car slot))
		      (map (cdr slot)))
		  (if (> dl-warn-conflict-verbosity 2)
		      (message "Examining keys of map:\n    `%s'" map))
		  (map-keymap (lambda (key binding)
				(let* ((key (vconcat prefix (vector key)))
				       (binding (key-binding key)))
				  (if (and binding
					   (> dl-warn-conflict-verbosity 0))
				      (progn
					(if (not (keymapp binding))
					    (setq found (1+ found)))
					(message dl-conflict-warning
						 key binding)))))
			      map)))
	      (accessible-keymaps map))
      (if (> dl-warn-conflict-verbosity 1)
	  (message "Checking for conflicting bindings...done%s"
		   (if (> found 0)
		       (format ".  Found %d." found)
		     "."))))))

;; Register the mode with Emacs
;; `add-minor-mode' doesn't exist in Emacs 20.2  :-(
(or (assq 'dl-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons (list 'dl-mode dl-indicator-string) minor-mode-alist)))
(dl-mode-set-prefix dl-mode-prefix)

;;; end of dictionary lookup minor mode

(defvar dui-warn-previously-registered-methods-p t
  "Warn about previously registered methods.")

;;     [SJT:  OK, ispell uses M-$, and LEIM and Wnn both use C-\.  I see
;;	all three processes (spell-check, localized input methods, and
;;	dictionary lookup) as being aspects of high-level dictionary-
;;	based natural language input.  I would like to overload the same
;;	mode-toggle for all of them.  I see IMs as being primary (analogous
;;	to a minor mode), while the checking functions are secondary and/or
;;	transient.  Unfortunately, both ispell and LEIM use prefix args to
;;	modify the toggle's behavior.  But all of "C-$", "C-c $", "C-c \",
;;	and "C-c \" are undefined.
;;	  I see the interface as follows.
;;	The main-entry point is the the transient function (ispell-word,
;;	edict-lookup-*), accessed via the un-prefixed key.
;;	The prefixed key would give a short choice menu, in the echo area.
;;	A short-list of defaults would be alternative transient functions,
;;	plus the choices to add or delete from the menu, or to do more
;;	complicated maintenance (eg, customize, once we have an interface.)
;;
;; #### Need to define the call interface for the functions.
(defvar dui-method-history nil
  "List of recently used dictionary methods.")

;; Maybe would like to do something like the following?
;An alist containing elements of the form (METHOD &rest LISTS).
;
;METHOD is a unique string naming the dictionary method.  Each element
;of LISTS is a list of the form (TYPE DESCRIPTION INVOCATION &rest
;ARGS), where TYPE is a symbol (one of 'search, 'insert, or 'edit)
;indicating the context where this invocation is used, DESCRIPTION is a
;string describing this method, INVOCATION is a function to call to
;invoke this method, and the function will be applied to (cons TYPE
;ARGS).
(defvar dui-method-alist nil
  "Registry of dictionary methods and utilities.

An alist containing elements of the form (METHOD TYPE DESCRIPTION
INVOCATION &rest ARGS).

METHOD is a unique string naming the dictionary method.  TYPE is a
symbol (one of 'search, 'insert, or 'edit) indicating the context
where this invocation is used, DESCRIPTION is a string describing this
method, INVOCATION is a function to call to invoke this method, and
the function will be applied to (cons TYPE ARGS).")

;; Method component access functions
(defun dui-get-method-name (slot) (nth 0 slot))
(defun dui-get-method-type (slot) (nth 1 slot))
(defun dui-get-method-description (slot) (nth 2 slot))
(defun dui-get-method-invocation (slot) (nth 3 slot))
(defun dui-get-method-args (slot) (nthcdr 4 slot))

(defvar dui-registration-errors nil
  "String containing description of method registration problems.")

;; Flush any old errors hanging around.
(setq dui-registration-errors nil)

(defun dui-register-method
  (method type invocation description &rest args)
  "Register a dictionary method.

METHOD is a unique string naming the dictionary method.  TYPE
indicates the context in which the method is used (a symbol, one of
'search, 'insert, or 'edit).  INVOCATION is a function to call to
invoke this method, which is applied to ARGS.  DESCRIPTION is a string
describing this method.  The same INVOCATION function may be
registered in different contexts with different descriptions and
argument lists, but must have a different METHOD name in each context.

It may be useful to include METHOD as an element of ARGS to allow the
INVOCATION function to be used by several slightly different methods."
  (if (assoc method dui-method-alist)
      (setq dui-registration-errors
	    (concat dui-registration-errors
		    (format "%s\n" method)))
    (setq dui-method-alist
	  (cons (append (list method type description invocation) args)
		dui-method-alist))))

;; #### should this filter on type?  probably not.
(defun dui-remove-method (method)
  "Remove a dictionary method from the registry."
  (interactive (completing-read "Remove method: " dui-method-alist nil t))
  (setq dui-method-alist
	(delete (assoc method dui-method-alist)
		dui-method-alist)))

(defun dui-filter (type list)
  "Return the sub-list of methods from LIST whose type is TYPE."
  (apply 'append
	 (mapcar #'(lambda (method)
		     (if (eq (dui-get-method-type (assoc method dui-method-alist)) type)
			 (list method)
		       nil))
		 list)))

(defun dui-read-method (type prompt &optional default)
  "Read the name of a dictionary method from the minibuffer.

If DEFAULT is non-nil, use that as the default, substituting it into
PROMPT at the first `%s'.

Signals an error on null input.  The return value is a string."
  (if default (setq prompt (format prompt default)))
  (let* ((completion-ignore-case t)
	 ;; This binding is necessary if dui-method-history
	 ;; is buffer local.  For the name of the variable, see comments
	 ;; in lisp/minibuf.el on `read-from-minibuffer'; it's dynamic
         ;; scope lossage.
	 (naming-this-symbol-simply-history-loses
	  (dui-filter 'search dui-method-history))
	 ;; Ah, bogosity. Oberhasli croaks with wta listp, history.
	 ;; For now leave it in anyway.
	 (method (completing-read prompt dui-method-alist nil t nil
				  'naming-this-symbol-simply-history-loses)))
	 ;;(method (completing-read prompt dui-method-alist nil t)))
    (if (and (> (length method) 0)
	     (eq (dui-get-method-type (assoc method dui-method-alist)) type))
	method
      (error "No valid method was specified"))))

;; #### Make a method for defining additional keymap entries for folks
;;      who want secondary dictionaries available.
(defun dui-invoke-method (type ask)
  "Invoke a dictionary method.

With ASK non-nil, read a method of type TYPE from the minibuffer and
invoke it.

With ASK null, invoke the last selected method, if there is one,
otherwise read from minibuffer and invoke."

  (let* ((default (car (dui-filter type dui-method-history)))
	 (method (if (or ask (not default))
		     (dui-read-method
		      type
		      (if default
			  "Method (default %s): "
			"Method: ")
		      default)  
		   default))
	 (slot (assoc method dui-method-alist))
	 (invocation (dui-get-method-invocation slot))
	 (args (dui-get-method-args slot)))
    (setq dui-method-history
	  (cons method (delete method dui-method-history)))
    (apply invocation args)))

;; #### `dui-invoke-insert-method' and  `dui-invoke-edit-method' probably
;;      don't need defaults or histories.  Instead, they should be part
;;      of the information associated with the search method and they
;;      should be automatically invoked depending on the success or
;;      failure of the search mehtod.  (Insert methods should only be
;;      invoked if the appropriate user variable is set.)
;;;###autoload
(defun dui-invoke-search-method (ask)
  "Invokes a dictionary lookup method.

If ASK is non-nil, reads a method from the minibuffer.  Otherwise invokes the
current default search method.

\\[dui-describe-method] gives help for individual methods."
  (interactive "P")
  (dui-invoke-method 'search ask))

;;;###autoload
(defun dui-invoke-insert-method (ask)
  "Invokes a method to add a dictionary entry.

If ASK is non-nil, reads a method from the minibuffer.  Otherwise invokes the
current default insert method.

\\[dui-describe-method] gives help for individual methods."
  (interactive "P")
  (dui-invoke-method 'insert ask))

;;;###autoload
(defun dui-invoke-edit-method (ask)
  "Invokes a dictionary editing method.

If ASK is non-nil, reads a method from the minibuffer.  Otherwise invokes the
current default edit method.

\\[dui-describe-method] gives help for individual methods."
  (interactive "P")
  (dui-invoke-method 'edit ask))

;;;###autoload
(defun dui-describe-method (method)
  "Shows the docstring for METHOD (a string) in a temporary buffer."
  (interactive (completing-read "Describe method: " dui-method-alist nil t))
  (with-output-to-temp-buffer
      (princ (dui-get-method-description method))))

(defun dui-princ-errors ()
  (if (and dui-warn-previously-registered-methods-p
	   dui-registration-errors)
      (progn
	(princ "Methods are already registered by the following names.
If you wish to register a new method under one of these names, please use
`dui-remove-method' first.
")
	(princ dui-registration-errors)
	(setq dui-registration-errors nil))))

(provide 'dui)

;; load up the default methods
;; must come after the provide call to dui
(require 'dui-registry)

;;; dui.el ends here
