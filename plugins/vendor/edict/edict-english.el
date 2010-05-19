;;; edict-english.el --- English morphology rules for edict.el

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

;; English morphological rules

;;; To do:

;;; Changelog:

;; 1998-03-27  Stephen Turnbull  <turnbull@sk.tsukuba.ac.jp>
;;        (created):  broken out from monolithic edict.el

;;; Code:

(provide 'edict-english)

(require 'edict-morphology)

(define-edict-rule english-plural
  "\\([^i][^e]\\|i[^e]\\|[^i]e\\)\\(s\\)$"
  (english english-noun)
  edict-subst-affix edict-identity edict-ignore)

(define-edict-rule english-plural-ies
  "\\(ies\\)$"
  (english english-noun)
  edict-subst-affix "y")

;;; edict-english.el ends here
