;;; edict-japanese.el --- Japanese morphology rules for edict.el

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

;; Japanese morphological rules

;;; To do:

;;; Changelog:

;; 1998-03-27  Stephen Turnbull  <turnbull@sk.tsukuba.ac.jp>
;;        (created):  broken out from monolithic edict.el

;;; Code:

(provide 'edict-japanese)

(require 'edict-morphology)

;; Strip "います"
(define-edict-rule 「います」を削除する
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([いきぎしちにびみり]\\)\\(ま\\(す\\|せん\\)\\)$")
  "ませる$"
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (1 2)
  edict-ignore ())

(define-edict-rule 「ます」を削除する
  (concat "\\(" edict-category-c
	  "\\|[いきぎしちにびみりえけげせてねべめれ]\\)\\(ま\\(す\\|せん\\)\\)$")
  "ませる$"
  edict-subst-affix edict-identity "る")

(define-edict-rule 「来ます」の特別ルール
  "\\(来ま\\(す\\|せん\\)\\)$"
  ()
  edict-subst-affix "来る")

(define-edict-rule 「きます」の特別ルール
  "\\(^\\|て\\|んで\\)\\(きま\\(す\\|せん\\)\\)$"
  "ませる$"
  edict-subst-modified-affix
  edict-identity ()
  edict-subst ("くる"))

(define-edict-rule 「します」の特別ルール
  "\\(しま\\(す\\|せん\\)\\)$"
  ()
  edict-subst-affix "する")

;; The several cases of て／って.
;;  Note either pattern may generate multiple possibilities.
;; Also, た.
(define-edict-rule 「て／た」から「う」まで変換する
  "\\(っ\\(て\\|た[ら]?\\)\\)$" 
  ()
  edict-subst-affix "う")

(define-edict-rule 「て／た」から「つ」まで変換する
  "\\(っ\\(て\\|た[ら]?\\)\\)$" 
  ()
  edict-subst-affix "つ")

(define-edict-rule 「て／た」から「る」まで変換する
  "\\(っ\\(て\\|た[ら]?\\)\\)$" 
  ()
  edict-subst-affix "る")

(define-edict-rule 一段の「て／た」から「る」まで変換する
  (concat "\\(" edict-category-c
	  "\\|[いきぎしちにびみりえけげせてねべめれ]\\)\\(\\(て\\|た[ら]?\\)\\)$")
  ()
  edict-subst-affix edict-identity "る")

(define-edict-rule 「て／た」から「す」まで変換する
  "\\(し\\(て\\|た[ら]?\\)\\)$" 
  ()
  edict-subst-affix "す")

(define-edict-rule 「て／た」から「く」まで変換する
  "\\(い\\(て\\|た[ら]?\\)\\)$" 
  ()
  edict-subst-affix "く")

(define-edict-rule 「て／た」から「ぐ」まで変換する
  "\\(い[でだ]\\)$" 
  ()
  edict-subst-affix "ぐ")

(define-edict-rule 「て／た」から「ぶ」まで変換する
  "\\(ん\\(で\\|だ[ら]?\\)\\)$" 
  ()
  edict-subst-affix "ぶ")

(define-edict-rule 「て／た」から「む」まで変換する
  "\\(ん\\(で\\|だ[ら]?\\)\\)$" 
  ()
  edict-subst-affix "む")

(define-edict-rule 「て／た」から「ぬ」まで変換する
  "\\(ん\\(で\\|だ[ら]?\\)\\)$" 
  ()
  edict-subst-affix "ぬ")

;; 行く is an irregular verb.
(define-edict-rule 行くの特別ルール
  "行\\(っ\\(て\\|た[ら]?\\)\\)$"
  ()
  edict-subst-affix "く")

(define-edict-rule 「来て」の特別ルール
  "来\\(て\\|た[ら]?\\)$"
  ()
  edict-subst-affix "来る")

(define-edict-rule 「きて」の特別ルール
  "\\(きて\\|きた[ら]?\\)$"
  ()
  edict-subst-affix "くる")

(define-edict-rule 「して」の特別ルール
  "\\(して\\|した[ら]?\\)$"
  ()
  edict-subst-affix "する")

;; Potential form.
;; The filters here are due to 「一段の「て／た」から「る」まで変換する」
(define-edict-rule れる
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(れる\\)$")
  "れて$"
  edict-subst-affix edict-identity "る")

(define-edict-rule ける
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(ける\\)$")
  "けて$"
  edict-subst-affix edict-identity "く")

(define-edict-rule せる
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(せる\\)$")
  "せて$"
  edict-subst-affix edict-identity "す")

(define-edict-rule てる
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(てる\\)$")
  "\\(て\\|てられる\\)$"
  edict-subst-affix edict-identity "つ")

(define-edict-rule ねる
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(ねる\\)$")
  "ねて"
  edict-subst-affix edict-identity "ぬ")

(define-edict-rule める
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(める\\)$")
  "めて"
  edict-subst-affix edict-identity "む")

(define-edict-rule え
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(える\\)$")
  "えて"
  edict-subst-affix edict-identity "う")

(define-edict-rule げる
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(げる\\)$")
  "けて"
  edict-subst-affix edict-identity "ぐ")

(define-edict-rule べる
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(べる\\)$")
  "べて"
  edict-subst-affix edict-identity "ぶ")

;; 一段動詞。 Also serves for the passive.
(define-edict-rule られる
  (concat "\\(" edict-category-c
	  "\\|[いきぎしちにびみりえけげせてねべめれ]\\)\\(られる\\)$")
  ()
  edict-subst-affix edict-identity "る")

;; Passive
(define-edict-rule 五段動詞の「あれる」を変換する 
  "\\([わかがさたなまばら]\\)\\(れる\\)$"
  ()
  edict-subst-modified-affix
  edict-modify-verb (0 2)
  edict-ignore ())

(define-edict-rule 来られるのルール
  "来\\(られる\\)$"
  ()
  edict-subst-affix "る")

(define-edict-rule されるのルール
  "\\(される\\)$"
  ()
  edict-subst-affix "する")

;; Causative
(define-edict-rule 五段動詞の「あせる」を変換する 
  "\\([わかがさたなまばら]\\)\\(せる\\)$"
  ()
  edict-subst-modified-affix
  edict-modify-verb (0 2)
  edict-ignore ())

(define-edict-rule 一段動詞の「あせる」を変換する 
  (concat "\\(" edict-category-c
	  "\\|[いきぎしちにびみりえけげせてねべめれ]\\)\\(させる\\)$")
  ()
  edict-subst-affix edict-identity "る")

(define-edict-rule させるのルール
  "\\(させる\\)$"
  ()
  edict-subst-affix "する")

;; eba conditional form.
(define-edict-rule 「えば」を変換する "\\([えけげせてねべめれ]\\)\\(ば\\)$"
  ()
  edict-subst-modified-affix
  edict-modify-verb (3 2)
  edict-ignore ())

;; tara conditional form is handled as part of the て／た／たら rules.

;; The informal negative form.
(define-edict-rule 「ない」を変換する "\\([わかがさたなまばら]\\)\\(ない\\|ず\\)$"
  ()
  edict-subst-modified-affix
  edict-modify-verb (0 2)
  edict-ignore ())

(define-edict-rule 一段の「ない」を変換する
  (concat "\\(" edict-category-c
	  "\\|[いきぎしちにびみりえけげせてねべめれ]\\)\\(ない\\|ず\\)$")
  ()
  edict-subst-affix edict-identity "る")

(define-edict-rule 「しない」の特別ルール
  "\\(しない\\|せず\\)$"
  ()
  edict-subst-affix "する")

(define-edict-rule 「ない」の特別ルール
  "^\\(ない\\)$"
  ()
  edict-subst-affix "ある")

;; Conjunctive form

(define-edict-rule 一段のconjunctive
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)[いきぎしちにびみりえけげせてねべめれ]\\(\\)$")
  "く$\\|かった$\\|くる$\\|くれる$\\|ください$\\|あげる$\\|上げる$\\|しまう$\\|くて$\\|くない$\\|ければ$\\|いる$\\|からず$\\|います$\\|ある$\\|みる$\\|下さい$\\|なさい$\\|やる$\\|もらう$"
  edict-subst-modified-affix
  edict-identity ()
  edict-subst ("る"))

(define-edict-rule 五段のconjunctive
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([いきぎしちにびみり]\\)$")
  "く$\\|かった$\\|くる$\\|くれる$\\|ください$\\|あげる$\\|上げる$\\|しまう$\\|くて$\\|くない$\\|ければ$\\|いる$\\|からず$\\|います$\\|ある$\\|みる$\\|下さい$\\|なさい$\\|やる$\\|もらう$"
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (1 2))

(define-edict-rule 「する」の特別conjunctive
  (concat "\\(" edict-category-Japanese-word-constituent "\\)\\(し\\)$")
  "す$"
  edict-subst-affix edict-identity "する")

(define-edict-rule 「じる」の特別conjunctive
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(じ\\)$")
  ()
  edict-subst-affix edict-identity "じる")

(define-edict-rule 「ずる」の特別conjunctive
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(じ\\)$")
  ()
  edict-subst-affix edict-identity "ずる")

;; The informal imperative form, 五段動詞
(define-edict-rule 「れ」の五段動詞を変換する 
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([えけげせてねべめれ]\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (3 2))

;; The informal imperative form, 一段動詞
(define-edict-rule 「ろ」の一段動詞を変換する
  (concat "\\(" edict-category-c
	  "\\|[いきぎしちにびみりえけげせてねべめれ]\\)\\(ろ\\)$")
  ()
  edict-subst-affix edict-identity "る")

;; Irregulars
(define-edict-rule 「来い」の特別ルール
  "^\\(来い\\)$"
  ()
  edict-subst-affix "来る")

(define-edict-rule 「こい」の特別ルール
  "^\\(こい\\)$"
  "く$"
  edict-subst-affix "くる")

(define-edict-rule 「しろ」の特別ルール
  "^\\(しろ\\)$"
  ()
  edict-subst-affix "する")

;; The plain desiderative
(define-edict-rule 「たい」を削除する 
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([いきぎしちにびみり]\\)\\(たい\\|たがる\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (1 2)
  edict-ignore ())

(define-edict-rule 一段の「たい」を削除する
  (concat "\\(" edict-category-c
	  "\\|[いきぎしちにびみりえけげせてねべめれ]\\)\\(たい\\|たがる\\)$")
  ()
  edict-subst-affix edict-identity "る")

(define-edict-rule 「したい」の特別ルール
  "^\\(したい\\|したがる\\)$"
  ()
  edict-subst-affix "する")

(define-edict-rule 「来たい」の特別ルール
  "^\\(来たい\\|来たがる\\)$"
  ()
  edict-subst-affix "来る")

(define-edict-rule 「きたい」の特別ルール
  "^\\(きたい\\|きたがる\\)$"
  ()
  edict-subst-affix "くる")

;; Flush auxilliary verbs after te form.
(define-edict-rule 助動詞ー１
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\(く\\|て\\|んで\\)\\(いる\\|おる\\|います\\|ある\\|おく\\|みる\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule 助動詞ー１ａ
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\(て\\|んで\\)\\(る\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule 助動詞ー２
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\(く\\|て\\|んで\\)\\(下さい\\|ください\\|なさい\\|いく\\|行く\\|くる\\|来る\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule 助動詞ー３
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\(く\\|て\\|んで\\)\\(\\([さ差]し\\)?[あ上]げる\\|やる\\|もらう\\|いただく\\|頂く\\|くれる\\|くださる\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule 助動詞ー４
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\(く\\|て\\|んで\\)\\(する\\|成る\\|なる\\|しまう\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule modifiers
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)[いたうくぐすつぬぶむる]\\(らしい\\|そう\\|よう\\)$")
  ()
  edict-subst-affix edict-identity "")

(define-edict-rule humble
  (concat "\\(お\\)\\(" edict-category-c  "\\|" edict-category-h
	  "\\)+\\([いきぎしちにびみり]\\)\\(に成る\\|になる\\|する\\|いたす\\|申し上げる\\|もうしあげる\\)$")
  ()
  edict-subst-modified-affix
  edict-ignore ()
  edict-identity ()
  edict-modify-verb (1 2)
  edict-ignore ())

;; Volitional
(define-edict-rule 五段の「おう」
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([おこごそとのぼもろ]\\)\\(う\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (4 2)
  edict-ignore ())

(define-edict-rule 一段の「よう」
  (concat "\\(" edict-category-c
	  "\\|[いきぎしちにびみりえけげせてねべめれ]\\)\\(よう\\)$")
  ()
  edict-subst-affix edict-identity "る")

(define-edict-rule 「来よう」の特別ルール
  "\\(来よう\\)$"
  ()
  edict-subst-affix "来る")

(define-edict-rule 「こよう」の特別ルール
  "\\(こよう\\)$"
  ()
  edict-subst-affix "くる")

(define-edict-rule 「しよう」の特別ルール
  "\\(しよう\\)$"
  ()
  edict-subst-affix "する")

(define-edict-rule てしまう
  "[^ん]\\(ちゃう\\)$"
  ()
  edict-subst-affix "てしまう")

(define-edict-rule でしまう
  "ん\\(ちゃう\\)$"
  ()
  edict-subst-affix "でしまう")

;; Honorific prefixes
(define-edict-rule 敬語の接頭辞
  "^\\(お\\|御\\|ご\\)"
  ()
  edict-subst-affix "")

;; Various forms of adjectives.
(define-edict-rule 形容詞ーく
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(く\\)$")
  "\\(か\\(れる\\|せる\\|ない\\|ず\\)\\|き\\(ます\\|ません\\|たい\\|なから\\|つつ\\|やさい\\|にくい\\|そうな\\)\\|け\\(ば\\|\\|る\\)\\|こう\\|い\\(た\\|たら\\|たり\\|たろう\\|て\\|ている\\)\\)$"
  edict-subst-affix edict-identity "い")

(define-edict-rule 形容詞ーくて
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(くて\\)$")
  ()
  edict-subst-affix edict-identity "い")

(define-edict-rule 形容詞ーくない
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(くない\\)$")
  ()
  edict-subst-affix edict-identity "い")

(define-edict-rule 形容詞ーからず
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(からず\\)$")
  ()
  edict-subst-affix edict-identity "い")

(define-edict-rule 形容詞ーかった
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(かった\\)$")
  ()
  edict-subst-affix edict-identity "い")

(define-edict-rule 形容詞ーない
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\(\\(じゃ\\|では\\)\\(ない\\|ありません\\)\\)$")
  ()
  edict-subst-affix edict-identity "")

(define-edict-rule 形容詞ーければ
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(ければ\\)$")
  ()
  edict-subst-affix edict-identity "い")

;; Other affixes

(define-edict-rule other-suffixes
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\(的\\|てき\\|もの\\|物\\|者\\|式\\|中\\|員\\|する\\|さん\\|先生\\|様\\|さま\\|ちゃん\\|君\\|くん\\|屋\\)$")
  ()
  edict-subst-affix edict-identity "")

(define-edict-rule other-prefixes
  (concat "^\\(昨\\|来\\|全\\|半\\|毎\\)" edict-category-c)
  ()
  edict-subst-affix "")

;; Canonicalize number expressions
(define-edict-rule numbers
  (concat "^\\([0-9０-９一二三四五六七八九十百千万億]+\\)\\("
	  edict-category-c "\\|" edict-category-h "\\)")
  ()
  edict-subst-affix "一" edict-identity )

(define-edict-rule 数なし
  (concat "^\\([0-9０-９一二三四五六七八九十百千万億]+\\)\\("
	  edict-category-c "\\|" edict-category-h "\\)")
  ()
  edict-subst-affix edict-ignore edict-identity )

(define-edict-rule だ
  "\\(じゃない\\|ではない\\|だった\\|だろう\\)$"
  ()
  edict-subst-affix "だ")

(define-edict-rule です
  "\\(じゃありません\\|ではありません\\|でしょう\\)$"
  ()
  edict-subst-affix "です")

(define-edict-rule です/だ
  "\\(です\\)$"
  ()
  edict-subst-affix "だ")

(define-edict-rule copula
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\(だ\\|です\\)$")
  ()
  edict-subst-affix edict-identity edict-ignore)

;;; edict-japanese.el ends here
