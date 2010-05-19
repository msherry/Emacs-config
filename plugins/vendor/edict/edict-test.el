;;;;;; Copyright (C) 1992 Bob Kerns <rwk@crl.dec.com>
;;;
;;;
;;;   This program is free software; you can redistribute it and/or modify
;;;   it under the terms of the GNU General Public License as published by
;;;   the Free Software Foundation; either version 1, or (at your option)
;;;   any later version.  ;;; 
;;;   This program is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;   GNU General Public License for more details.  ;;; 
;;;   You should have received a copy of the GNU General Public License
;;;   along with this program; if not, write to the Free Software
;;;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  ;;; 

;;; Test suite for morphology rules for edict.el.
;;; To run the tests, load this file, and do m-X edict-perform-tests.
;;; This will create an *EDICT-TESTS* buffer with the results.


(require 'cl)

;;; This should exist, but doesn't.  See edict.install for the
;;; compiler half of this.  You should be sure to load the same
;;; hacks into your compiler if you compile this by hand, or you
;;; won't get it byte compiled.

;(defmacro eval-when (when &rest forms)
;  (and (or (member 'eval when)
;	    (member ':execute when))
;       (mapcar (function eval) forms))
;  (and (or (member 'load when)
;	   (member ':load-toplevel when))
;       (cons 'progn forms)))

;;; first, a couple simple tests.

(defun edict-test-string (flag string)
  "Show what strings will be searched for a test string.  If given a prefix arg,
traces step by step; type Return for each new step."
  (interactive "P
sTest string: ")
  (let ((*edict-expand-string-trace* flag))
    (message (format "%s" (edict-expand-string string)))))

(defun edict-test-rule (rule-name string)
  (interactive "SRule name: 
sTest string: ")
  (let ((rule (edict-get-rule rule-name)))
    (unless rule (error "There is no rule named '%s'" rule-name))
    (unless (string-match (edict-rule-pattern rule) string)
      (error "The rule %s does not match '%s'." rule-name string))
    (apply (edict-rule-function rule) string
	   (edict-rule-additional-args rule))))

(eval-when (eval load compile)
(defstruct edict-test
  word					; Word to be tested.
  should-have				; Expansions that should be found
  should-not-have			; Expansions that should not be found.
  from-syntax-types
  to-syntax-types)
)

(defvar *edict-tests* nil)

(defun remove-edict-test (name)
  (let ((test (get-edict-test name)))
    (setq *edict-tests* (delq test *edict-tests*))))

(defun add-edict-test (test)
  ;; Preserve the order of the tests.
  (let* ((name (edict-test-word test))
	 (old (get-edict-test name)))
    (if old
	(setf (edict-test-should-have old) (edict-test-should-have test)
	      (edict-test-should-not-have old) (edict-test-should-not-have test)
	      (edict-test-from-syntax-types old)
	      (edict-test-from-syntax-types test)
	      (edict-test-to-syntax-types old)
	      (edict-test-to-syntax-types test))
      (setq *edict-tests* (append *edict-tests* (list test))))))

(defun get-edict-test (name)
  (if (symbolp name)
      (setq name (symbol-name name)))
  (catch 'found-it
    (dolist (test *edict-tests*)
      (if (equal (edict-test-word test) name)
	  (throw 'found-it test)))))

(defmacro deftest (case &optional fromto should-have should-not-have not-self)
  (` (define-edict-test '(, case) '(, (first fromto)) '(, (second fromto))
       '(, should-have) '(, should-not-have) '(, not-self))))

(defun define-edict-test (name from to should-have should-not-have
			       &optional not-self)
  (if (symbolp name)
      (setq name (symbol-name name)))
  (unless (listp from)
    (setq from (list from)))
  (unless (listp to)
    (setq to (list to)))
  (unless from
    (setq from '(日本語)))
  (let ((f (function (lambda (x)
		       (if (symbolp x)
			   (symbol-name x)
			 x)))))
    (setq should-have (mapcar f should-have))
    (setq should-not-have (mapcar f should-not-have))
    (or not-self (member name should-have)
	(push name should-have))
    (add-edict-test (make-edict-test :word name
				     :should-have should-have
				     :should-not-have should-not-have
				     :from-syntax-types from
				     :to-syntax-types to)))
  name)

;;; This should be in emacs, but it isn't.
;;; (Borrowed from ilisp.el, where I inherited it accidentally).

(defun edict-del (item list &optional test)
  "Delete ITEM from LIST using TEST comparison and return the result.
Default test is equal."
  (let ((test (or test (function equal)))
	(element list)
	(prev nil)
	(done nil))
    (while (and element (not done))
      (if (funcall test item (car element))
	  (progn
	    (setq done t)
	    (if prev
		(rplacd prev (cdr element))
		(setq list (cdr list))))
	  (setq prev element
		element (cdr element))))
    list))


(defun edict-test (test)
  (if (or (symbolp test) (stringp test))
      (setq test (get-edict-test test)))
  ;; Cleaning up the kanji shouldn't break anything;
  ;; give it a chance to do so if it's buggy.
  (let* ((name (edict-test-word test))
	 (word (edict-clean-up-kanji name))
	 (from-syntax-types (edict-test-from-syntax-types test))
	 (to-syntax-types (edict-test-to-syntax-types test))
	 (should-have (edict-test-should-have test))
	 (should-not-have (edict-test-should-not-have test)))
    (let* ((expansion (edict-expand-string-syntaxes word () () from-syntax-types))
	   (save-expansion expansion)
	   (failed nil))
      (dolist (sh should-have)
	(if (member sh expansion)
	    (setq expansion (edict-del sh expansion (function equal)))
	  (progn 
	    (princ (format ";%s: did not produce %s - %S\n" name sh save-expansion))
	    (setq failed t))))
      (dolist (case should-not-have)
	(and (member case expansion)
	     (progn
	       (princ (format ";%s: Should not have %s as expansion.\n"
			      name case))
	       (setq failed t)
	       (setq expansion (edict-del sh expansion (function equal))))))
      (dolist (bad expansion)
	(princ (format ";%s: Unexpected expansion: %s\n" name bad))
	(setq failed t))
      (or failed
	  (princ (format ";%s: OK\n" name)))
      (not failed))))

(defun edict-perform-tests ()
  (interactive)
  (let ((test-buffer (get-buffer-create "*EDICT-TESTS*"))
	(failures 0)
	(first-failure nil))
    (set-buffer test-buffer)
    (set-window-buffer (selected-window) test-buffer)
    (delete-region (point-min) (point-max))
    (let ((standard-output test-buffer))
      (dolist (test *edict-tests*)
	(let ((msg-point (point)))
	  (cond ((not (edict-test test))
		 (incf failures)
		 (or first-failure (setq first-failure msg-point))))
	  (sit-for 0))))
    (cond ((= failures 0)
	   (message "Done.  All Tests OK."))
	  ((= failures 1)
	   (message "1 test failed."))
	  (t (message (format "%d tests failed." failures))))
    (goto-char (or first-failure (point-min)))))

(defun edict-run-test (arg)
  "Execute the test that point is in or before.
Print value in minibuffer.
With argument, insert value in current buffer after the defun.
With argument >= 16 (i.e. c-U c-U), single-step through the expansion process."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point))
	  (*edict-expand-string-trace* (and arg (> (prefix-numeric-value arg) 4))))
      (beginning-of-defun)
      (let* ((test-form (read (current-buffer)))
	     (test-name (second test-form))
	     (test))
	(eval test-form)
	(setq test (get-edict-test test-name))
	(forward-line 1)
	(while (looking-at (concat ";" (symbol-name test-name)
				   ": \\(Unexpected expansion: \\|did not produce \\|OK$\\)"))
	  (let ((start (point)))
	    (forward-line 1)
	    (delete-region start (point))))
	(let ((standard-output (if arg (current-buffer) standard-output)))
	  (edict-test test)))))
  t)

;(global-set-key "\e_" 'edict-run-test)

;;; **** NOTE WELL ****
;;; The proper test results here are not necessarily valid words.
;;; These are words which are MORPHOLOGICALLY correct.  That is,
;;; this reverse-chains on the possible rules to produce a given
;;; word, generally only one or two of which would actually be
;;; correct.

;;; Also note that these are regression tests.  No distinction is being
;;; made between results which are "correct" and results which are
;;; "acceptable".  In general, we accept spurious expansions if they
;;; lead to including desirable results in other cases.  Modifying the
;;; rule set may either result in eliminating spurious expansions (resulting
;;; in missing expansions from the tests) or adding new spurious expansions.
;;; In case of problems from these tests, the offending test should be single-stepped
;;; (with c-u c-u m-X edict-run-test), and the reasons for the expansion should be
;;; evaluated.  If, after careful consideration, the modified result is regarded
;;; as correct, the test should be modified accordingly.  Otherwise, the bug should
;;; be fixed.

;;; Be careful.  Regression tests are good for considering all the effects of
;;; a change, but they do not themselves determine the correctness of a change.
;;; When the regression tests determine that something has changed, it is up
;;; to YOU to be careful and determine the correct result.

(deftest "買う "
  ()
  (買う)
  ()
  :not-self)

(deftest "
＞　買＃#>!！：:う	 "
  ()
  (買う)
  ()
  :not-self)

;;; The basics: 五段動詞
(deftest 買う
  ())
(deftest 行く
  ()
  (行い))				;Looks like it could be an adverb
;行く: OK
(deftest 泳ぐ
  ())
(deftest 話す
  ())
(deftest 待つ
  ())
(deftest 死ぬ
  ())
(deftest 呼ぶ
  ())
(deftest 読む
  ())
(deftest 分かる
  ())
(deftest 成る
  ())
;;; 一段動詞
(deftest 生きる
  ())
(deftest 見る
  ())

;;; Distal style
;;; These all produce the improbable but possible result of removing only the
;;; masu and adding る as if it were a 一段動詞, since the result of that situation
;;; would look the same.

(deftest 買います
  ()
  (買う 買いる))
(deftest 置きます
  ()
  (置く 置きる))
;置きます: OK
(deftest 泳ぎます
  ()
  (泳ぐ 泳ぎる))
(deftest 話します
  ()
  (話す 話しる 話する 話))
(deftest 持ちます
  ()
  (持つ 持ちる))
(deftest 死にます
  ()
  (死ぬ 死にる))
(deftest 呼びます
  ()
  (呼ぶ 呼びる))
(deftest 読みます
  ()
  (読む 読みる))
(deftest 分かります
  ()
  (分かる 分かりる))
(deftest 成ります
  ()
  (成る 成りる))
(deftest 生きます
  ()
  (生きる 生く))
;生きます: OK
(deftest 見ます
  ()
  (見る))


;;; Irregulars

(deftest 来ます
  ()
  (来る))
(deftest きます
  ()
  (くる きる))
(deftest します
  ()
  (する しる))

(deftest 買いません
  ()
  (買う 買いる))
(deftest 置きません
  ()
  (置く 置きる))
;置きません: OK
(deftest 泳ぎません
  ()
  (泳ぐ 泳ぎる))
(deftest 話しません
  ()
  (話す 話しる 話する 話))
(deftest 持ちません
  ()
  (持つ 持ちる))
(deftest 死にません
  ()
  (死ぬ 死にる))
(deftest 呼びません
  ()
  (呼ぶ 呼びる))
(deftest 読みません
  ()
  (読む 読みる))
(deftest 分かりません
  ()
  (分かる 分かりる))
(deftest 成りません
  ()
  (成る 成りる))
(deftest 生きません
  ()
  (生きる 生く))
;生きません: OK
(deftest 見ません
  ()
  (見る))


;;; Irregulars

(deftest 来ません
  ()
  (来る))
(deftest きません
  ()
  (くる きる))
(deftest しません
  ()
  (する しる))


;;; Past tense

(deftest 買った
  ()
  (買う 買つ 買る))
(deftest 置いた
  ()
  (置く 置いる))
;置いた: OK
(deftest 行った
					();iku is irregular It looks like a る/つ/う.
  (行く 行い 行う 行つ 行る))
;行った: OK
(deftest 話した
  ()
  (話す 話しる 話する 話))
;話した: OK
(deftest 持った
  ()
  (持つ 持う 持る))
(deftest 死んた
					();Don't mis-interpret
  ()
  (死ぬ))
(deftest 死んだ
  ()
  (死ぬ 死ぶ 死む 死ん))
;死んだ: OK
(deftest 呼んだ
  ()
  (呼ぶ 呼む 呼ぬ 呼ん))
;呼んだ: OK
(deftest 読んだ
  ()
  (読む 読ぬ 読ぶ 読ん))
;読んだ: OK
(deftest 分かった
  ()
  (分かる 分い 分かう 分かつ))
;分かった: OK
(deftest 成った
  ()
  (成る 成う 成つ))
;;; 一段動詞
(deftest 生きた
  ()
  (生きる 生くる))
;生きた: OK
(deftest 見た
  ()
  (見る))

;;; Gerund
;;; These all also map to つ, because of the plan imperative form.
;;; This seems surprising, if you're not thinking about it.

(deftest 買って
  ()
  (買う 買つ 買る 買っつ 買ってる))
;買って: OK
(deftest 置いて
  ()
  (置く 置いる 置いつ 置いてる))
;置いて: OK
(deftest 行って
					();iku is irregular It looks like a る/つ/う.
  (行く 行い 行う 行つ 行る 行っつ 行ってる))
;行って: OK
(deftest 話して
  ()
  (話す 話しる 話しつ 話する 話 話してる))
;話して: OK
(deftest 持って
  ()
  (持つ 持う 持る 持っつ 持ってる))
;持って: OK
(deftest 死んて
					();Don't mis-interpret
  (死んつ 死んてる)
  (死ぬ))
;死んて: OK
(deftest 死んで
  ()
  (死ぬ 死ぶ 死む))
;死んで: OK
(deftest 呼んで
  ()
  (呼ぶ 呼む 呼ぬ))
;呼んで: OK
(deftest 読んで
  ()
  (読む 読ぬ 読ぶ))
(deftest 分かって
  ()
  (分かる 分かう 分かつ 分かっつ 分かってる))
;分かって: OK
(deftest 成って
  ()
  (成る 成う 成つ 成っつ 成ってる))
;成って: OK
;;; 一段動詞
(deftest 生きて
  ()
  (生きる 生きつ 生くる 生きてる))
;生きて: OK
(deftest 見て
  ()
  (見る 見つ 見てる))
;見て: OK

;;; Potential

(deftest 買える
  ()
  (買う))
;買える: OK

(deftest 食べられる
  ()
  (食べる 食べらる 食ぶ))
;食べられる: OK

(deftest 呼べる
  ()
  (呼ぶ))
;呼べる: OK

;;; Passive
;;; These also look like they could be 一段どうし potentials.

(deftest 買われる
  ()
  (買う 買わる))
;買われる: OK

(deftest 置かれる
  ()
  (置く 置かる))
;置かれる: OK

(deftest 泳がれる
  ()
  (泳ぐ 泳がる))
(deftest 話される
  ()
  (話す 話する 話さる 話))		;Because of irregular する
(deftest 待たれる
  ()
  (待つ 待たる))
(deftest 死なれる
  ()
  (死ぬ 死なる))
(deftest 読まれる
  ()
  (読む 読まる))
;読まれる: OK
(deftest 呼ばれる
  ()
  (呼ぶ 呼ばる))
(deftest 見られる
  ()
  (見る 見らる))

;;; Irregulars
(deftest 来られる
  ()
  (来る 来らる))
(deftest される
  ()
  (する さる す))			;す because of the regular rule.

;;; Causitive

(deftest 買わせる
  ()
  (買う 買わす))
;買わせる: OK
(deftest 置かせる
  ()
  (置く 置かす))
;置かせる: OK
(deftest 泳がせる
  ()
  (泳ぐ 泳がす))
;泳がせる: OK
(deftest 話させる
  ()
  (話る 話す 話する 話さす 話))		;Because of irregular する
;話させる: OK
(deftest 待たせる
  ()
  (待つ 待たす))
;待たせる: OK
(deftest 死なせる
  ()
  (死ぬ 死なす))
;死なせる: OK
(deftest 読ませる
  ()
  (読む 読ます))
;読ませる: OK
(deftest 呼ばせる
  ()
  (呼ぶ 呼ばす))
;呼ばせる: OK
(deftest 見させる
  ()
  (見る 見す 見する 見さす 見))		;Because of regular & irregular rules
;見させる: OK

;;; Irregulars
(deftest 来させる
  ()
  (来る 来す 来する 来さす 来))		;because of regular & irregular rules.
;来させる: OK
(deftest させる
  ()
  (する さす す))			;す because of the regular rule.
;させる: OK

;;; Conditional

(deftest 買えば
  ()
  (買う))
(deftest 置けば
  ()
  (置く))
(deftest 泳げば
  ()
  (泳ぐ))
(deftest 話せば
  ()
  (話す))
(deftest 待てば
  ()
  (待つ))
(deftest 死ねば
  ()
  (死ぬ))
(deftest 読めば
  ()
  (読む))
(deftest 呼べば
  ()
  (呼ぶ))
(deftest 見れば
  ()
  (見る))

;;; たら conditional form

(deftest 買ったら
  ()
  (買う 買つ 買る))
(deftest 置いたら
  ()
  (置く 置いる))
(deftest 行ったら
					();iku is irregular It looks like a る/つ/う.
  (行く 行い 行う 行つ 行る))
(deftest 話したら
  ()
  (話す 話しる 話する 話))
;話したら: OK
(deftest 持ったら
  ()
  (持つ 持う 持る))
(deftest 死んたら
					();Don't mis-interpret
  ()
  (死ぬ))
(deftest 死んだら
  ()
  (死ぬ 死ぶ 死む))
(deftest 呼んだら
  ()
  (呼ぶ 呼む 呼ぬ))
(deftest 読んだら
  ()
  (読む 読ぬ 読ぶ))
(deftest 分かったら
  ()
  (分かる 分かう 分かつ))
(deftest 成ったら
  ()
  (成る 成う 成つ))
;;; 一段動詞
(deftest 生きたら
  ()
  (生きる 生くる))
;生きたら: OK
(deftest 見たら
  ()
  (見る))

;;; Plain negative

(deftest 買わない
  ()
  (買う 買わない 買わなう 買わないる))
;買わない: OK
(deftest 置かない
  ()
  (置く 置かない 置かなう 置かないる))
;置かない: OK
(deftest 泳がない
  ()
  (泳ぐ 泳がないる 泳がなう))
;泳がない: OK
(deftest 話さない
  ()
  (話す 話さないる 話さなう))
;話さない: OK
(deftest 待たない
  ()
  (待つ 待たないる 待たなう))
;待たない: OK
(deftest 死なない
  ()
  (死ぬ 死なないる 死ななう))
;死なない: OK
(deftest 読まない
  ()
  (読む 読まないる 読まなう))
;読まない: OK
(deftest 呼ばない
  ()
  (呼ぶ 呼ばないる 呼ばなう))
;呼ばない: OK
(deftest 見ない
  ()
  (見る 見ないる 見なう))
;見ない: OK

;;; Irregulars
(deftest 来ない
  ()
  (来る 来ないる 来なう))
;来ない: OK
(deftest しない
  ()
  (する しる しないる しなう))		;しる because of regular rules.
;しない: OK
(deftest ない
  ()
  (ある ないる なう))
;ない: OK

;;; ず negatives

(deftest 買わず
  ()
  (買う))
;買わず: OK
(deftest 置かず
  ()
  (置く))
;置かず: OK
(deftest 泳がず
  ()
  (泳ぐ))
;泳がず: OK
(deftest 話さず
  ()
  (話す))
;話さず: OK
(deftest 待たず
  ()
  (待つ))
;待たず: OK
(deftest 死なず
  ()
  (死ぬ))
;死なず: OK
(deftest 読まず
  ()
  (読む))
;読まず: OK
(deftest 呼ばず
  ()
  (呼ぶ))
;呼ばず: OK
(deftest 見ず
  ()
  (見る))
;見ず: OK

;;; Irregulars
(deftest 来ず
  ()
  (来る))
;来ず: OK
(deftest せず
  ()
  (する せる))				;せる because of regular rules.
;せず: OK


;;; Plain command form

(deftest 買え
  ()
  (買う 買える))

(deftest 置け
  ()
  (置く 置ける))
;置け: OK
(deftest 泳げ
  ()
  (泳ぐ 泳げる))
(deftest 話せ
  ()
  (話す 話せる))
(deftest 待て
  ()
  (待つ 待て 待る 待てる))
;待て: OK
(deftest 死ね
  ()
  (死ぬ 死ねる))
(deftest 読め
  ()
  (読む 読める))
(deftest 呼べ
  ()
  (呼ぶ 呼べる))
(deftest 見ろ
  ()
  (見る))

;;; Irregulars
(deftest 来い
  ()
  (来る 来いる 来う))
;来い: OK
(deftest こい
  ()
  (くる こいる こう))
;こい: OK
(deftest しろ
  ()
  (する しる))				;しる because of regular rules.

;;; The plain desideratives

(deftest 買いたい
  ()
  (買う 買いる 買いたいる 買いたう))
;買いたい: OK
(deftest 置きたい
  ()
  (置く 置きる 置きたいる 置きたう))
;置きたい: OK
(deftest 泳ぎたい
  ()
  (泳ぐ 泳ぎる 泳ぎたいる 泳ぎたう))
;泳ぎたい: OK
(deftest 話したい
  ()
  (話す 話しる 話したいる 話したう))
;話したい: OK
(deftest 持ちたい
  ()
  (持つ 持ちる 持ちたいる 持ちたう))
;持ちたい: OK
(deftest 死にたい
  ()
  (死ぬ 死にる 死にたいる 死にたう))
;死にたい: OK
(deftest 呼びたい
  ()
  (呼ぶ 呼びる 呼びたいる 呼びたう))
;呼びたい: OK
(deftest 読みたい
  ()
  (読む 読みる 読みたいる 読みたう))
;読みたい: OK
(deftest 分かりたい
  ()
  (分かる 分かりる 分かりたいる 分かりたう))
;分かりたい: OK
(deftest 成りたい
  ()
  (成る 成りる 成りたいる 成りたう))
;成りたい: OK
(deftest 生きたい
  ()
  (生きる 生く 生きたいる 生きたう))
;生きたい: OK
(deftest 見たい
  ()
  (見る 見たいる 見たう))
;見たい: OK


;;; Irregulars

(deftest 来たい
  ()
  (来る 来たいる 来たう))
;来たい: OK
(deftest きたい
  ()
  (くる きる きたいる きたう))
;きたい: OK
(deftest したい
  ()
  (する しる したいる したう))
;したい: OK

(deftest 買いたがる
  ()
  (買う 買いる))
(deftest 置きたがる
  ()
  (置く 置い 置きる))
(deftest 泳ぎたがる
  ()
  (泳ぐ 泳ぎる))
(deftest 話したがる
  ()
  (話す 話しる))
(deftest 持ちたがる
  ()
  (持つ 持ちる))
(deftest 死にたがる
  ()
  (死ぬ 死にる))
(deftest 呼びたがる
  ()
  (呼ぶ 呼びる))
(deftest 読みたがる
  ()
  (読む 読みる))
(deftest 分かりたがる
  ()
  (分かる 分かりる))
(deftest 成りたがる
  ()
  (成る 成りる))
(deftest 生きたがる
  ()
  (生きる 生く 生い))			; Could be an adverb or adjective.
(deftest 見たがる
  ()
  (見る))


;;; Irregulars

(deftest 来たがる
  ()
  (来る))
(deftest きたがる
  ()
  (くる きる))
(deftest したがる
  ()
  (する しる))


;;; Here's a compound test.

(deftest 行きたがっています
  ()
  (行く 行きたがって 行きたがる 行きたがう 行きたがつ
   行きたがっつ 行きたがっている 行きたがってう 行い
   行きる))
;行きたがっています: OK

(deftest 読んでいる
  ()
  (読んで 読む 読ぬ 読ぶ))
;読んでいる: OK
(deftest 買っている
  ()
  (買って 買う 買っつ 買つ 買る))
;買っている: OK

(deftest 読んでいた
  ()
  (読んで 読む 読ぬ 読ぶ 読んでいる 読んでく))
;読んでいた: OK
(deftest 買っていた
  ()
  (買って 買う 買っつ 買つ 買る 買っている 買ってく))
;買っていた: OK

(deftest 読んでいます
  ()
  (読んで 読む 読ぬ 読ぶ 読んでいる 読んでう))
;読んでいます: OK
(deftest 買っています
  ()
  (買って 買う 買っつ 買つ 買る 買っている 買ってう))
;買っています: OK

(deftest 読んである
  ()
  (読んで 読む 読ぬ 読ぶ))
;読んである: OK
(deftest 買ってある
  ()
  (買って 買う 買っつ 買つ 買る))
;買ってある: OK

(deftest 読んでおく
  ()
  (読んで 読む 読ぬ 読ぶ 読んでおい))
;読んでおく: OK
(deftest 買っておく
  ()
  (買って 買う 買っつ 買つ 買る 買っておい))
;買っておく: OK

(deftest 読んでみる
  ()
  (読んで 読む 読ぬ 読ぶ))
;読んでみる: OK
(deftest 買ってみる
  ()
  (買って 買う 買っつ 買つ 買る))
;買ってみる: OK

(deftest 読んでしまう
  ()
  (読んで 読む 読ぬ 読ぶ))
;読んでしまう: OK
(deftest 買ってしまう
  ()
  (買って 買う 買っつ 買つ 買る))
;買ってしまう: OK

(deftest 読んでください
  ()
  (読んで 読む 読ぬ 読ぶ 読んでくださいる 読んでくださう))
;読んでください: OK
(deftest 買ってください
  ()
  (買って 買う 買っつ 買つ 買る 買ってくださいる 買ってくださう))
;買ってください: OK

(deftest 読んで下さい
  ()
  (読んで 読む 読ぬ 読ぶ 読んで下さいる 読んで下さう))
;読んで下さい: OK
(deftest 買って下さい
  ()
  (買って 買う 買っつ 買つ 買る 買って下さいる 買って下さう))
;買って下さい: OK

(deftest 読んでなさい
  ()
  (読んで 読む 読ぬ 読ぶ 読んでなさいる 読んでなさう))
;読んでなさい: OK
(deftest 買ってなさい
  ()
  (買って 買う 買っつ 買つ 買る 買ってなさいる 買ってなさう))
;買ってなさい: OK

(deftest 読んでいく
  ()
  (読んで 読む 読ぬ 読ぶ 読んでいい))
;読んでいく: OK
(deftest 買っていく
  ()
  (買って 買う 買っつ 買つ 買る 買っていい))
;買っていく: OK

(deftest 読んでくる
  ()
  (読んで 読む 読ぬ 読ぶ))
;読んでくる: OK
(deftest 買ってくる
  ()
  (買って 買う 買っつ 買つ 買る))
;買ってくる: OK

(deftest 読んであげる
  ()
  (読んで 読む 読ぬ 読ぶ 読んであぐ))
;読んであげる: OK
(deftest 買ってあげる
  ()
  (買って 買う 買っつ 買つ 買る 買ってあぐ))
;買ってあげる: OK

(deftest 読んでやる
  ()
  (読んで 読む 読ぬ 読ぶ))
;読んでやる: OK
(deftest 買ってやる
  ()
  (買って 買う 買っつ 買つ 買る))
;買ってやる: OK

(deftest 読んでもらう
  ()
  (読んで 読む 読ぬ 読ぶ))
;読んでもらう: OK
(deftest 買ってもらう
  ()
  (買って 買う 買っつ 買つ 買る))
;買ってもらう: OK

(deftest 読んでいただく
  ()
  (読んで 読む 読ぬ 読ぶ 読んでいただい))
;読んでいただく: OK
(deftest 買っていただく
  ()
  (買って 買う 買っつ 買つ 買る 買っていただい))
;買っていただく: OK

(deftest 読んでくれる
  ()
  (読んで 読む 読ぬ 読ぶ 読んでくる))
;読んでくれる: OK
(deftest 買ってくれる
  ()
  (買って 買う 買っつ 買つ 買る 買ってくる))
;買ってくれる: OK

(deftest 読んでいただきます
  ()
  (読んで 読む 読ぬ 読ぶ 読んでいただく 読んでいただきる))
;読んでいただきます: OK
(deftest 買っていただきます
  ()
  (買って 買う 買っつ 買つ 買る 買っていただく 買っていただきる))
;買っていただきます: OK

(deftest 買って頂きます
  ()
  (買って 買う 買っつ 買つ 買る 買って頂く 買って頂きる))
;買って頂きます: OK

(deftest 読んでください
  ()
  (読んで 読む 読ぬ 読ぶ 読んでくださう 読んでくださいる))
;読んでください: OK
(deftest 買ってください
  ()
  (買って 買う 買っつ 買つ 買る 買ってくださう 買ってくださいる))
;買ってください: OK

(deftest 読んで上げる
  ()
  (読んで 読む 読ぬ 読ぶ 読んで上ぐ))
;読んで上げる: OK
(deftest 買ってあげる
  ()
  (買って 買う 買っつ 買つ 買る 買ってあぐ))
;買ってあげる: OK
(deftest 読んで差し上げる
  ()
  (読んで 読む 読ぬ 読ぶ 読んで差し上ぐ))
;読んで差し上げる: OK
(deftest 買って差し上げる
  ()
  (買って 買う 買っつ 買つ 買る 買って差し上ぐ))
;買って差し上げる: OK

(deftest 買って差しあげる
  ()
  (買って 買う 買っつ 買つ 買る 買って差しあぐ))
;買って差しあげる: OK
(deftest 買ってさしあげる
  ()
  (買って 買う 買っつ 買つ 買る 買ってさしあぐ))
;買ってさしあげる: OK
(deftest 買ってさし上げる
  ()
  (買って 買う 買っつ 買つ 買る 買ってさし上ぐ))
;買ってさし上げる: OK

(deftest 読むらしい
  ()
  (読む 読むらしう 読むらしいる))
;読むらしい: OK

(deftest 読むそう
  ()
  (読む 読むす))
;読むそう: OK

(deftest 読むよう
  ()
  (読む))
;読むよう: OK

(deftest 読むようだ
  ()
  (読む 読むよう))
;読むようだ: OK

(deftest 買おう
  ()
  (買う))
;買おう: OK
(deftest 置こう
  ()
  (置く))
;置こう: OK
(deftest 泳ごう
  ()
  (泳ぐ))
;泳ごう: OK
(deftest 話そう
  ()
  (話す))
;話そう: OK
(deftest 待とう
  ()
  (待つ))
;待とう: OK
(deftest 死のう
  ()
  (死ぬ))
;死のう: OK
(deftest 読もう
  ()
  (読む))
;読もう: OK
(deftest 呼ぼう
  ()
  (呼ぶ))
;呼ぼう: OK
(deftest 見よう
  ()
  (見る))
;見よう: OK

;;; Irregulars
(deftest 来よう
  ()
  (来る))
;来よう: OK
(deftest こよう
  ()
  (くる))
;こよう: OK
(deftest しよう
  ()
  (する しる))				;しる due to the regular rules.
;しよう: OK

(deftest 読んちゃう
  ()
  (読んでしまう 読んで 読む 読ぬ 読ぶ))
;読んちゃう: OK
(deftest 買っちゃう
  ()
  (買ってしまう 買って 買う 買っつ 買つ 買る))
;買っちゃう: OK

(deftest 読んちゃった
  ()
  (読んでしまう 読んで 読む 読ぬ 読ぶ
   読んちゃう 読んちゃる 読んちゃつ))
;読んちゃった: OK

(deftest 買っちゃった
  ()
  (買ってしまう 買って 買う 買っつ 買つ 買る
   買っちゃう 買っちゃる 買っちゃつ))
;買っちゃった: OK

(deftest 削除する
  ()
  (削除))
;削除する: OK

;;; Honorific prefixes

(deftest お水
  ()
  (水))
;お水: OK

(deftest ご飯
  ()
  (飯))
;ご飯: OK

(deftest 御飯
  ()
  (飯))
;御飯: OK

;;; Adjectives

(deftest 新しく
  ()
  (新しい))
;新しく: OK

(deftest 新しくて
  ()
  (新しい 新しくつ 新しくてる))
;新しくて: OK

(deftest 新しかった
  ()
  (新しい 新しかう 新しかつ 新しかる))
;新しかった: OK

(deftest 元気ではありません
  ()
  (元気 元気ではある 元気ではありる 元気だ 元気です))
;元気ではありません: OK

(deftest 元気ではない
  ()
  (元気 元気ではないる 元気ではなう 元気だ))
;元気ではない: OK

(deftest 元気じゃありません
  ()
  (元気 元気じゃある 元気じゃありる 元気だ 元気です))
;元気じゃありません: OK

(deftest 元気じゃない
  ()
  (元気 元気じゃないる 元気じゃなう 元気だ))
;元気じゃない: OK

(deftest 新しくなくて
  ()
  (新しい 新しくない 新しくなくつ 新しくなくてる))
;新しくなくて: OK

(deftest 新しければ
  ()
  (新しい 新しく 新しける))
;新しければ: OK

(deftest 新しくない
  ()
  (新しい 新しくなう 新しくないる))
;新しくない: OK

(deftest 勉強中
  ()
  (勉強))
;勉強中: OK

(deftest 結婚式
  ()
  (結婚))
;結婚式: OK

(deftest 忘れもの
  ()
  (忘れ 忘れる 忘る))
;忘れもの: OK

(deftest 忘れ物
  ()
  (忘れ 忘れる 忘る))
;忘れ物: OK

(deftest 旅行者
  ()
  (旅行))
;旅行者: OK

(deftest 館員
  ()
  (館))
;館員: OK

(deftest 昨日
  ()
  (日))
;昨日: OK

(deftest 来年
  ()
  (年))
;来年: OK

(deftest 全国
  ()
  (国))
;全国: OK

;;; Humble

(deftest お飲みになります
  ()
  (飲む お飲みになる お飲みになりる お飲みになります 
   飲みになります 飲みになる 飲みになりる))
;お飲みになります: OK

(deftest お飲みに成ります
  ()
  (飲む お飲みに成る お飲みに成りる 
   飲みに成ります 飲みに成る 飲みに成りる))
;お飲みに成ります: OK

(deftest 行ってきます
  ()
  (行く 行って 行ってくる 行ってく 行ってきる
   行う 行つ 行る 行い 行っつ))
;行ってきます: OK

(deftest 分割しないよう
  ()
  (分割 分割する 分割しる 分割しない 分割しないる 分割しなう))
;分割しないよう: OK

(deftest 滞在している
  ()
  (滞在 滞在する 滞在して 滞在しつ 滞在す 滞在しる))
;滞在している: OK

(deftest 長くなります
  ()
  (長く 長い 長くなる 長くなりる))
;長くなります: OK

;;; これは分かっていません：
;;  >  |これでxinfoで日本語が表示できます．Emacsのinfoは使いずらかったので，
;;  >  |xinfoの存在はとてもありがたいと思います．
;; 
;; 「使いずらかった」とは何ですか。
;; →「使いず」は、「使わず」ですか。
;; →「使わなかったら」ですか。
;; では、「使いず」と「らかった」と「ずら」をedict がわからなった。
;; 私の先生に尋ねよう。

(deftest 少なからず
  ()
  (少ない 少なかる 少る))
;少なからず: OK

;;; Test the various titles.

(deftest 甕吉実先生
  ()
  (甕吉実))
;甕吉実先生: OK

(deftest 中村さん
  ()
  (中村))
;中村さん: OK

(deftest 宮本ちゃん
  ()
  (宮本))
;宮本ちゃん: OK

(deftest 林君
  ()
  (林))
;林君: OK

(deftest 小沢くん
  ()
  (小沢))
;小沢くん: OK

(deftest 星野様
  ()
  (星野))
;星野様: OK

(deftest 古坂さま
  ()
  (古坂))
;古坂さま: OK

;;; Test the various number cases.

(deftest 二人
  ()
  (一人 人))
;二人: OK

(deftest 17人
  ()
  (一人 人))
;17人: OK

(deftest １７人
  ()
  (一人 人))
;１７人: OK

;;; This one caused infinite recursion, due to a hole in the
;;; redundant-expansion checking (things didn't get checked for redundancy
;;; quite soon enough, so short cycles weren't detected).

(deftest 出て
  ()
  (出る 出てる 出つ))
;出て: OK

;;; This one caused infinite recursion, due to failure to root certain
;;; patterns.  I've since added checks on the patterns to enforce rootedness.

(deftest 通じる
  ()
  ())
;通じる: OK

(deftest ２種類
  ()
  (一種類 種類))
;２種類: OK

(deftest あかいじゃありません
  ()
  (あかいです あかいじゃある あかいじゃありる あかいだ あかぐ あかう あかいる あかい))
;あかいじゃありません: OK

(deftest 雨でしょう
  ()
  (雨です 雨だ 雨))
;雨でしょう: OK

(deftest 猫s
  ()
  ()
  (猫))
;猫s: OK

(deftest 形容詞ー
  ()
  (形容詞))

(deftest keys
  (english)
  (key))
;keys: OK

(deftest families
  (english)
  (family))
;families: OK

(provide 'edict-test)
