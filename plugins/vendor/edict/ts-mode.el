;;; ts-mode.el --- Insert time-stamps in buffers

;; Copyright (C) 1998 by Stephen J. Turnbull

;; Author:      Stephen J. Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Keywords:    minor mode
;; Version:     1.0
;; Created:     Sun Apr  5 19:49:36 1998

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

;; Insert a timestamp in a buffer, implemented as a minor mode.
;; Written to try out some techniques in the implementation of minor
;; modes, in particular courteous key-binding.

;;; To do:

;; 1.  Support Custom.

;;; Changelog:

;; 1998-04-05  Stephen Turnbull  <turnbull@sk.tsukuba.ac.jp>
;;        tm-mode.el:  created

;;; Code

(provide 'ts-mode)

;;; User customization variables

(defvar ts-mode-prefix '[(control ?c) (?\$)]
  "Prefix key sequence for ts-mode command keys.

After loading, change the mode's prefix by using ts-mode-set-prefix;
setq'ing this variable can't work.")

;; A convention for modes; here honored by observance, not breach.
(defvar ts-mode-hook nil
  "If you can think of a use for this, you're more clever than I.")

;; Auxiliary customizations

(defvar ts-conflict-warning "Binding conflict: %s -> %s."
  "Format string warning about key sequences with conflicting bindings.

Must contain two `%s' descriptors.  The first formats the key sequence,
the second the description of the existing binding.")

(defvar ts-warn-conflict-verbosity 3
  "Controls verbosity of binding conflict warnings.

0   turns off warnings entirely.
1   issues a warning for each binding conflict (including sub-keymaps).
2   prints a summary message with a count of conflicts (not including
    sub-keymaps, only keys in those maps that have conflicts).
3   adds verbose detail about what is being done.

Each positive level performs all actions of lower levels.")

;;; The basic mode conventions.

;; Mode flag and keymaps
;;
(defvar ts-mode nil "Activation flag for ts-mode.")

(defvar ts-mode-submap nil
  "Sub-keymap for ts-mode.

All key sequences are prefixed by the sequence defined in ts-mode-map.")

(defvar ts-mode-map nil
  "Keymap for ts-mode.  Holds the prefix key for ts-mode functions.

Its value will be installed into minor-mode-map-alist.  Prefix cannot
be altered by setq'ing ts-mode-map.  Use ts-mode-set-prefix instead.")

;; Mode toggle
;;
(defun ts-mode (&optional arg)
  "Minor mode for inserting time stamps in buffers.

An example minor mode.

\\{ts-mode-map}"

  (interactive "P")

  ;; ts-warn-binding-conflicts doesn't make sense in the mode
  (if (null ts-mode)
      (ts-warn-binding-conflicts ts-mode-map))
  (setq ts-mode (if (null arg) (not ts-mode)
                  (> (prefix-numeric-value arg) 0)))
  (run-hooks ts-mode-hook))

;;; Internal mode data

(or ts-mode-submap
    (progn
     (setq ts-mode-submap (make-sparse-keymap))
     (define-prefix-command 'ts-mode-submap 'ts-mode-submap)
     (define-key ts-mode-submap ?T 'ts-timestamp)
     (define-key ts-mode-submap ?s 'ts-timestamp)
     ))

;;; Helper functions

;; Set the mode prefix
;;
;; This can't be done simply by setq'ing ts-mode-map; minor-mode-map-alist
;; does not refer to that variable but contains a copy of its value.
;;
(defun ts-mode-set-prefix (key &optional context)
  "Set the prefix key sequence for ts-mode to KEY.

Return the new ts-mode-map.  When called interactively read KEY from
the minibuffer (as a string; keys not bound to `self-insert' must be
quoted with ^Q).  If you need more flexibility than ASCII gives, you'll
have to use the `eval-expression' interface. 

Allowed values of CONTEXT:
NIL                substitute a map containing KEY in minor-mode-map-alist.
adding-minor-mode  manipulation of minor-mode-map-alist is done elsewhere."
  ;; Should read key events but I don't know how to make that work.
  (interactive "Key sequence (quote control characters with ^Q): ")
  (setq ts-mode-map (make-sparse-keymap))
  (define-key ts-mode-map key 'ts-mode-submap)
  (cond ((null context)
	 (let ((slot (assq 'ts-mode minor-mode-map-alist)))
	   (setq minor-mode-map-alist
		 (cons (cons 'ts-mode ts-mode-map)
		       (if slot
			   (delete slot minor-mode-map-alist)
			 minor-mode-map-alist)))))
	((equal context 'adding-minor-mode))
	(t (error "Illegal context `%s' in ts-mode-set-prefix." context)))
  ts-mode-map)

;; Deal with binding conflicts
;;
;; Search keymaps for binding conflicts for each key in the mode's keymap.
;; Current implementation searches only active keymaps; it won't tell
;; about inactive keymaps, including those of minor modes that might be
;; invoked later or (worse) major modes already invoked in other buffers.
;;
(defun ts-warn-binding-conflicts (map)
  "Warn about key bindings that will conflict with those in MAP.

Results will be non-sensical if MAP is invoked via a prefix or is
already active.  The current implementation only looks in the active
keymaps.  Maps of inactive minor modes and local maps major modes of
other buffers will not be searched (although the latter will be shadowed
since ts-mode is a global variable)."
  (let ((found 0))
    (if (> ts-warn-conflict-verbosity 1)
	(progn
	  (message "Checking for conflicting bindings...")
	  (if (> ts-warn-conflict-verbosity 2)
	      (message "Examining accessible maps of map:\n    `%s'" map))))
    ;; A map is accessible from itself
    (mapcar (lambda (slot)
	      (let ((prefix (car slot))
		    (map (cdr slot)))
		(if (> ts-warn-conflict-verbosity 2)
		    (message "Examining keys of map:\n    `%s'" map))
		(map-keymap (lambda (key binding)
			      (let* ((key (vconcat prefix (vector key)))
				     (binding (key-binding key)))
				(if (and binding
					 (> ts-warn-conflict-verbosity 0))
				    (progn
				      (if (not (keymapp binding))
					  (setq found (1+ found)))
				      (message ts-conflict-warning
					       key binding)))))
			    map)))
	    (accessible-keymaps map))
    (if (> ts-warn-conflict-verbosity 1)
	(message "Checking for conflicting bindings...done%s"
		 (if (> found 0)
		     (format ".  Found %d." found)
		   ".")))))

;;; Define a trivial command for a trivial example mode.

(defun ts-timestamp ()
  "Insert the current time string in the current buffer at point."
  (interactive)
  (insert (current-time-string)))

;;; Register the mode with XEmacs

(add-minor-mode 'ts-mode
		" ts"
		(ts-mode-set-prefix ts-mode-prefix 'adding-minor-mode))

;;; end of ts-mode.el
