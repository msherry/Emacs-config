;;; edict-morphology.el --- morphology rewrite engine for edict.el

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

;; Morphology rewrite engine

;;; To do:

;;; Changelog:

;; 1998-03-27  Stephen Turnbull  <turnbull@sk.tsukuba.ac.jp>
;;        (created):  broken out from monolithic edict.el

;;; Code:

(require 'cl)				; for defstruct

;;; Constants:

;; The edict-category-* variables are used to emulate the character
;; categories for regexps that are (partially) documented, but not
;; implemented, in XEmacs/Mule (20.5).  It should be possible to use
;; ranges for this, in the sense that defined legal characters in a
;; given range are guaranteed to be of the appropriate category.  The
;; reason is that each Mule-defined character set will occupy such a
;; range by virtue of the leading-byte implementation (see `Info |
;; Internals | MULE Character Sets and Encodings | Internal Mule
;; Encodings | Internal Character Encoding' for the easily understood
;; character type representation; the Bufbyte representation is a
;; simple transformation format of varying width).  Then JIS, at
;; least, carefully arranges the categories of characters into
;; non-overlapping ranges (ranges > 96 code points (94 for JIS) are
;; necessarily non-contiguous, see the info section cited above---this
;; is why undefined or illegal characters cannot be ruled out).  Mule
;; itself is happy to insert undefined characters---try eval'ing
;; (insert (int-char (+ (char-int ?$Bt$(B) 3)))---but balks at illegal
;; ones---(insert (int-char (- (char-int ?$B0!(B) 1))).  However, there
;; are two holes in JIS X 0208 (between the yomi-ordered Level 1 kanji
;; and the radical-ordered Level 2, and at the end of the character
;; set) and these undefined characters can be inserted, eg by the LISP
;; code above.

;; Regexp ranges cannot be depended on; they work for all JIS-derived
;; encodings (including EUC, ISO-2022-JP, and SJIS), but won't for
;; UCS/Unicode.  Watch out for (eg) Big-5 in trying to generalize this
;; code.

;; The values for the ranges are taken from Ken Lunde, Understanding
;; Japanese Information Processing, (C) 1993 O'Reilly & Associates,
;; Sebastopol, CA, and from $B6S8+!&9b66!&8MB<!&H>ED!&7,M}!&8~@n!&5HED!"(B
;; $B%^%k%A%j%s%,%k4D6-$N<B8=!"%W%l%s%F%#%9%[!<%k=PHG(B.

(eval-and-compile
  (defconst edict-category-c "[$B0!(B-$Bt&(B]"
    "JIS X 0208-1990 kanji character category.
NB: The last two characters of the range will be invisible for most fonts
(based on JIS X 0208-1983).")

  (defconst edict-category-C "[$(D0!(B-$(Dmc(B]"
    "JIS X 0212-1990 kanji character category.
Computed as kuten 1601 - kuten 7767, leading byte 0x94, but not verified
(I don't have the fonts on hand.)  #### Also, this may not be the official
definition of character category \cC in FSF Emacs 20.
Cannot be combined with JIS X 0208-1983/1990 (leading byte 0x92) since
Korean KSC 5601-1987 is interpolated (leading byte 0x93).")

  (defconst edict-category-h "[$B$!(B-$B$s(B]"
    "JIS X 0208-1990 hiragana character category.")

  (defconst edict-category-H "[$B~~(B]"
    "Unknown (half-width hiragana character?) category.
Currently implemented as the undefined character kuten 9494 in JIS X 0208.")

  (defconst edict-category-k "[$B%!(B-$B%t(B]"
    "JIS X 0208-1990 katakana character category.
Does not include the `small ka' and `small ke' characters (which would not
be appropriate in yomi).")

  (defconst edict-category-K "[(I&(B-(I_(B]"
    "JIS X 0201-1976 \"half-width\" katakana character category.")

  (defconst edict-category-kana
    (concat edict-category-h "\\|" edict-category-H
	    "\\|" edict-category-k "\\|" edict-category-K)
    "Japanese kana (half- or full-width, kata- or hira-) character category.")

  (defconst edict-category-Japanese-word-constituent
    (concat edict-category-kana "\\|" edict-category-c "\\|" edict-category-C)
    "Japanese word constituent character category.")

  (defconst edict-dictionary-entry-start-regexp
    (concat "^\\(\\(" edict-category-Japanese-word-constituent "\\)+\\)[ \t]")
    "Matches start of an EDICT entry.")

  (defconst edict-yomi-regexp
    (concat "^\\(" edict-category-kana "\\)+$")
    "Matches a pronunciation from an EDICT entry.")

  (defconst edict-yomi-part-regexp
    (concat "\\[^\\(" edict-category-kana "\\)+$")
    "Matches a `yomi part' (includes delimiters) from an EDICT entry.")
  )

;; The syntax of the rules is:
;; (define-edict-rule name <pattern> <conversion-function> <conversion-data>).
;; 
;;  <pattern> is a regular expression, with the parts to be substituted
;;  being denoted by \\(<subpattern>\\).
;; 
;;  <conversion function> is a funtion responsible for determining
;;  the replacements.  The current choices are
;;  edict-subst-modified-affix and edict-subst-affix.  These
;;  functions are called just after doing match-string, so the regexp
;;  variables are set up.  They are applied to the string, and
;;  <conversion-data>.  These functions are responsible for
;;  determining and performing the substitutions to be made, and
;;  returning a list of possiblities.
;; 
;;  edict-subst-affix is the simpler case.  It takes as conversion
;;  data one string for each subpattern in the pattern.  This string
;;  will be used in place of the original.
;; 
;;  edict-subst-modified-affix takes as conversion data, an
;;  alternating list of functions and lists of additional arguments
;;  for those functions.  Each function is applied to the substring
;;  being replaced and its additional arguments.  Likely functions to
;;  use include edict-modify-verb, edict-ignore, and edict-subst.

;; Table of morphological rules.
(defvar *edict-syntax-types* nil)

;; defstruct's defsetfs should expand into this; sigh.
;; Maybe this is fixed, comment them out.
;(eval-when (eval load compile)
(defstruct edict-syntax-type
  name
  rules)
;)

(defun get-edict-syntax-type (name)
  (if (symbolp name)
      (catch 'found-it
	(dolist (s *edict-syntax-types*)
	  (when (eq (edict-syntax-type-name s) name)
	    (throw 'found-it s)))
	(let ((new (make-edict-syntax-type :name name :rules ())))
	  (push new *edict-syntax-types*)
	  new))
    name))

;(eval-when (eval load compile)
(defstruct edict-rule
  name
  pattern				;Pattern which it must match
  filter				;Syntactic filter on previous form
  function				;Function to transform the input
  additional-args			;Arguments to transform function
  from-syntax-types			;Syntaxes for which this is valid
  to-syntax-types)			;Syntaxes to consider after this rule.
;)

;; Delete all occurrances of a rule from the rule base.
(defun edict-delete-rule (name)
  (dolist (s *edict-syntax-types*)
    (let ((old (edict-get-rule-from-syntax-type name s)))
      (when old
	(setf (edict-syntax-type-rules s)
	      (delq old (edict-syntax-type-rules s)))))))

;(defun edict-decircularize-rules ()
;  (interactive)
;  (dolist (s *edict-syntax-types*)
;    (dolist (r (edict-syntax-type-rules s))
;      (setf (edict-rule-from-syntax-types r)
;	    (mapcar (function (lambda (type)
;				(if (symbolp type)
;				    type
;				  (edict-syntax-type-name type))))
;		    (edict-rule-from-syntax-types r)))
;      (setf (edict-rule-to-syntax-types r)
;	    (mapcar (function (lambda (type)
;				(if (symbolp type)
;				    type
;				  (edict-syntax-type-name type))))
;		    (edict-rule-to-syntax-types r))))))
;
;(defun edict-circularize-rules ()
;  (interactive)
;  (dolist (s *edict-syntax-types*)
;    (dolist (r (edict-syntax-type-rules s))
;      (setf (edict-rule-from-syntax-types r)
;	    (mapcar (function (lambda (type)
;				(if (symbolp type)
;				    (get-edict-syntax-type type)
;				  type)))
;		    (edict-rule-from-syntax-types r)))
;      (setf (edict-rule-to-syntax-types r)
;	    (mapcar (function (lambda (type)
;				(if (symbolp type)
;				    (get-edict-syntax-type type)
;				  type)))
;		    (edict-rule-to-syntax-types r))))))

(defun edict-add-rule (name rule)
  (edict-delete-rule name)
  (dolist (s (edict-rule-from-syntax-types rule))
    (push rule (edict-syntax-type-rules s))))

(defun edict-get-rule-from-syntax-type (name syntax-type)
  (catch 'edict-get-rule
    (dolist (rule (edict-syntax-type-rules syntax-type))
      (if (eq name (edict-rule-name rule))
	  (throw 'edict-get-rule rule)))))

(defmacro define-edict-rule (name pattern fromto function &rest additional-args)
  ;; First, some type-checking.
  (let ((filter nil)
	(from nil)
	(to nil)
	;; SJT: this needs to be a string.  If it already is, that's ok.
	(pattern (eval pattern)))
    (when (stringp fromto)
      (setq filter fromto
	    fromto nil))
    (when (null fromto)
      (setq fromto '($BF|K\8l(B $BF|K\8l(B)))
    (setq from (first fromto)
	  to (second fromto))
    (unless (listp from)
      (setq from (list from)))
    (unless (listp to)
      (setq to (list to)))
    (unless (string-match "^\\^\\|\\$$" pattern)
      (error "Rule %s: pattern must start with ^ or end with $: \"%s\""
	     name pattern))
    (when filter
      (unless (stringp filter)
	(error "Rule %s: filter must be a regexp"
	       name)))
    (` (define-edict-rule-internal '(, name) '(, pattern) '(, filter)
	 '(, from) '(, to)
	 (function (, function)) (quote ((,@ additional-args)))))))

(defun define-edict-rule-internal (name pattern filter 
					from-syntax-types to-syntax-types
					function additional-args)
  (unless (string-match "^\\^\\|\\$$" pattern)
    (error "Rule %s: pattern must start with ^ or end with $: \"%s\""
	   name pattern))
  (when filter
    (unless (stringp filter)
      (error "Rule %s: filter must be a regexp"
	     name)))
  (let ((from-types nil)
	(to-types nil))
    (dolist (f from-syntax-types)
      (push (get-edict-syntax-type f) from-types))
    (dolist (to to-syntax-types)
      (push (get-edict-syntax-type to) to-types))
    (edict-add-rule name 
		    (make-edict-rule :name name
				     :pattern pattern
				     :filter filter
				     :from-syntax-types from-types
				     :to-syntax-types to-types
				     :function function
				     :additional-args additional-args))
    name))

;; #### This is bogus; the function does not match what the
;; #### description above the rules says it satisfies.  In particular, 
;; #### it is supposed to take only strings as arguments.
(defun edict-subst-affix (string &rest affixes)
  (let ((i 1)
	(prev -1)
	(result ""))
    (dolist (x affixes)
      (let ((pos (match-beginning i)))
	;; #### aren't the behaviors of edict-identity and
	;;      edict-ignore reversed?
	;; #### Hmm ... maybe this code never triggers.
	(cond ((eq x 'edict-identity))
	      ((eq x 'edict-ignore)
	       (setq result (concat result
				    (substring string
					       (max prev 0)
					       (match-beginning i)))
		     prev (match-end i)))
	      ((and (symbolp x) (fboundp x))
	       (setq result
		     (concat result
			     (substring string
					(max prev 0)
					(match-beginning i))
			     (funcall x (substring string
						   (match-beginning i)
						   (match-end i))))))
	      ((not (stringp x))
	       (error "%s is not a string or function name in edict-subst-affix"
		      x))
	      ((and pos (>= pos prev))
	       (setq result (concat result
				    (substring string
					       (max prev 0)
					       (match-beginning i))
				    x))
	       (setq prev (match-end i))))
	(incf i)))
    (concat result (substring string (max prev 0)))))

;; Takes a series of alternating pairs of substitution functions
;; and arguments for those substitution functions.  This can be
;; used to algorithmically replace certain parts (typically involving
;; changing an $B$$9T(B to $B$&9T(B final character.

(defun edict-subst-modified-affix (string &rest affixes)
  (let ((fun nil)
	(args nil)
	(i 1)
	(prev -1)
	(result ""))
    (while affixes
      (setq fun (car affixes)
	    args (car (cdr affixes))
	    affixes (cdr (cdr affixes)))
      (let ((pos (match-beginning i)))
	;; #### aren't the behaviors of edict-identity and
	;;      edict-ignore reversed?
	;; #### Hmm ... maybe this code never triggers.
	(cond ((eq fun 'edict-identity))
	      ((eq fun 'edict-ignore)
	       (setq result (concat result
				    (substring string
					       (max prev 0)
					       (match-beginning i)))
		     prev (match-end i)))
	      ((not (or (stringp fun) (and (symbolp fun) (fboundp fun))))
	       (error "%s is not a string or function name in %s"
		      fun 
		      'edict-subst-modified-affix))
	      ((and pos (>= pos prev))
	       (setq result (concat result
				    (substring string (max prev 0) pos)
				    (apply fun (substring string 
							  (match-beginning i)
							  (match-end i))
					   args)))
	       (setq prev (max prev (match-end i)))))
	(incf i)))
    (concat result (substring string (max prev 0)))))

;; Ignore this piece
(defun edict-ignore (affix) "")

;; Keep this piece
(defun edict-identity (affix) affix)

;; Substitute for this piece
(defun edict-subst (affix data)
  data)

;; More or less a guon table, for converting doshi suffixes.
(defvar *edict-doshi-suffix*
  '(["$B$o(B" "$B$$(B" "$B$&(B" "$B$((B" "$B$*(B"];; u -> wa; kau->kawanai
    ["$B$+(B" "$B$-(B" "$B$/(B" "$B$1(B" "$B$3(B"]
    ["$B$,(B" "$B$.(B" "$B$0(B" "$B$2(B" "$B$4(B"]
    ["$B$5(B" "$B$7(B" "$B$9(B" "$B$;(B" "$B$=(B"]
    ["$B$6(B" "$B$8(B" "$B$:(B" "$B$<(B" "$B$>(B"]
    ["$B$?(B" "$B$A(B" "$B$D(B" "$B$F(B" "$B$H(B"]
    ["$B$@(B" "$B$B(B" "$B$E(B" "$B$G(B" "$B$I(B"]
    ["$B$J(B" "$B$K(B" "$B$L(B" "$B$M(B" "$B$N(B"]
    ["$B$O(B" "$B$R(B" "$B$U(B" "$B$X(B" "$B$[(B"]
    ["$B$P(B" "$B$S(B" "$B$V(B" "$B$Y(B" "$B$\(B"]
    ["$B$Q(B" "$B$T(B" "$B$W(B" "$B$Z(B" "$B$](B"]
    ["$B$^(B" "$B$_(B" "$B$`(B" "$B$a(B" "$B$b(B"]
    ["$B$i(B" "$B$j(B" "$B$k(B" "$B$l(B" "$B$m(B"]))

(defun edict-modify-verb (suffix from to)
  (catch 'exit
    (dolist (b *edict-doshi-suffix*)
      (if (equal suffix (aref b from))
	  (throw 'exit (aref b to))))
    (throw 'skip-rule nil)))

;; Set this to true for debugging.
(defvar *edict-expand-string-trace* nil)  

;; This returns a list of the results of applying all rules whose
;; patterns match, to all levels of recursion.
(defun edict-expand-string (string &optional others previous syntax)
  (let* ((result nil)
	 (syntax (or syntax '$BF|K\8l(B))
	 (stype (get-edict-syntax-type syntax)))
    (dolist (rule (edict-syntax-type-rules stype))
      (when (string-match (edict-rule-pattern rule) string)
	(catch 'skip-rule
	  (unless (and previous
		       (edict-rule-filter rule)
		       (edict-filter-rule rule previous))
	    (let ((temp (apply (edict-rule-function rule) string
			       (edict-rule-additional-args rule))))
	      (unless (or (equal temp string)
			  (member temp others)
			  (member temp result))
		(when *edict-expand-string-trace*
		  (read-string (format "%s: %s -> %s -:" 
				       (edict-rule-name rule)
				       string temp)))
		(setq result
		      (union (edict-expand-string-recurse
				    temp (cons string (append result others))
				    string rule)
				   result))))))))
    (if (member string result)
	result
      (cons string result))))

(defun edict-expand-string-recurse (string others previous rule)
  (edict-expand-string-syntaxes string others previous 
				(edict-rule-to-syntax-types rule)))

(defun edict-expand-string-syntaxes (string others previous syntaxes)
  (let ((result nil))
    (dolist (syntax syntaxes)
      (setq result
	    (union (edict-expand-string string
					      (append result others)
					      previous
					      syntax)
			 result)))
    result))


;; Returns T if the rule should not be run, because of the past
;; history of expansions.  I.e. if something started out with $B$/(Bon
;; the end, and we've made it into an adjective, we should disable
;; any expansions based on it being a the conjunctive/stem form of a
;; verb.  This is done purely based on the most immediately preceding
;; expansion, because that is what determined the sense of the word.

(defun edict-filter-rule (rule previous)
  (let ((filter (edict-rule-filter rule)))
    (cond ((null filter) nil)
	  ((null previous) nil)
	  ((stringp filter)
	   (string-match filter previous))
;; #### This code is not functional yet, let those cases signal errors.
;	  ((symbolp filter)
;	   (funcall filter frob))
;	  ((consp filter)
;	   (apply (car filter) frob (cdr filter)))
	  (t (error "Bogus filter in rule %s: %s"
		    (edict-rule-name rule)
		    filter)))))

;(defun edict-find (elt list)
;  (catch 'edict-find
;    (dolist (test list)
;      (when (equal elt test)
;	(throw 'edict-find test)))))

;(defun edict-union (set1 set2)
;  (let ((result set2))
;    (dolist (frob set1)
;      (unless (member frob set2)
;	(setq result (cons frob result))))
;    result))

(provide 'edict-morphology)

;;; edict-morphology.el ends here
