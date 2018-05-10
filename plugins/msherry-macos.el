;;; Mac-OS specific configuration

;; Default on mac - "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"

;; Try this font for a while. Either syntax is acceptable here
(set-frame-font
 "Inconsolata-14"
 ;"-apple-Inconsolata-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
 )

;; While emacs23 handles greek poorly on the mac, use a different font. See
;; last http ref in plugins/lambda.el. "fontset-startup" (or fontsets at all,
;; it seems) don't exist in 22, so only do this on the mac for now
(set-fontset-font
 t
 ; used to use 'greek-iso8859-7, but that overwrote more
 ; than just the lambda character, so now we specify a range of 1
 ; char
 '(955 . 955)
 "-*-Andale Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; Use a decent japanese font for all kanji, hiragana, and katakana, rather
;; than a crap chinese font. It seems that it's kind of random when a font is
;; used, and can change, so we specify all three ranges manually. Obviously
;; we could combine hiragana and katakana ranges. これは二本語です
(mapc #'(lambda (x)
          (set-fontset-font "fontset-default"
                            x
                            "-apple-Osaka-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1"))
      '((?\x3040 . ?\x309F)           ; Hiragana
        (?\x30A0 . ?\x30FF)           ; Katakana
        (?\x4E00 . ?\x9FBF)           ; Kanji
        ))

(mapc #'(lambda (x)
          (set-fontset-font t
                            x
                            "-apple-Andale_Mono-medium-normal-normal-*-14-*-*-*-p-0-iso10646-1"))
      '((?\x898 . ?\x898)           ; ∀
        (?\x8A0 . ?\x8A0)           ; ∈
        ))


;;; Use https://github.com/leoliu/play-sound-osx to provide sound support on
;;; Mac OS
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))

;;; Set source directory for emacs
;;; https://stackoverflow.com/a/11595671/52550
(setq find-function-C-source-directory "~/Library/Caches/Homebrew/emacs--git/src")


(provide 'msherry-macos)
