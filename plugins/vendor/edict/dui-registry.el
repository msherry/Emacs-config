;;; dui-registry.el --- Registry of dui dictionary methods

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

;;; To do:

;;; Changelog:

;; 1998-04-08  Stephen Turnbull  <turnbull@sk.tsukuba.ac.jp>
;;        (created):  broken out from edict.el

;;; Code:

(require 'dui)

(dui-register-method
 "ispell word"
 'search
 'ispell-word
 "Invokes ispell on the word at point.

Heaven only knows what will happen if it's Japanese.")

(defun edict-search-english-wrapper ()
  "Interface `edict-search-english' to `dui-invoke-method'."
  (edict-search-english nil))

;; Probably could do without the wrapper:
;  (dui-register-method
;    "EDICT search english"
;    'search
;    'edict-search-english
;    "Attempts to translate the english word we are looking at. Picks the word 
;  in the same way as ispell, ie backs up from whitespace, and then expands.
;
;  Result is presented in a window that is not selected."
;    nil)

(dui-register-method
  "EDICT search English"
  'search
  'edict-search-english-wrapper
  "Attempts to translate the english word we are looking at. Picks the word 
in the same way as ispell, ie backs up from whitespace, and then expands.

Result is presented in a window that is not selected.")

(defun edict-search-kanji-wrapper ()
  "Interface `edict-search-kanji' to `dui-invoke-method'."
  (let ((m (mark))
	(p (point)))
    (cond
     ((null m)
      (error "Please set the region around the Japanese phrase to look up."))
     ((< m p) (edict-search-kanji nil m p))
     (t (edict-search-kanji nil p m)))))

(dui-register-method
  "EDICT search Japanese"
  'search
  'edict-search-kanji-wrapper
  "Attempts to translate the Japanese `word' between mark and point.

Verbs and adjectives will be deinflected, common auxiliaries and suffixes
removed, and all resulting candidates looked up.

Result is presented in a window that is not selected.")

;; Make it default
(or (featurep 'dui-registry)
    (setq dui-method-history
	  (cons "EDICT search Japanese" dui-method-history)))

(defun edict-add-kanji-wrapper ()
  "Interface `edict-add-kanji' to `dui-invoke-method'."
  (let ((m (mark))
	(p (point)))
    (cond
     ((null m)
      (error "Please mark the Japanese word to add to your private dictionary."))
     ((< m p) (edict-add-kanji m p))
     (t (edict-add-kanji p m)))))

(dui-register-method
  "EDICT add Japanese to private dictionary"
  'insert
  'edict-add-kanji-wrapper
  "Adds the Japanese `word' between mark and point to the private dictionary.

The entry is formatted for EDICT, and edict-edit-mode is entered.")

(dui-register-method
  "EDICT add English to private dictionary"
  'insert
  'edict-add-kanji-wrapper
  "Adds the English word near point to the private dictionary.

The entry is formatted for EDICT, and edict-edit-mode is entered.")

;(dui-princ-errors)

(provide 'dui-registry)

;;; dui-registry.el ends here
