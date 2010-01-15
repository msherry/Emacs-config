;; Marc Sherry

;; Make cyan roughly similar on linux and mac
(defvar cyan-name (cond ((eq system-type 'darwin) "cyan")
                          (t "brightcyan")))

;; Orchid is a terrible color for builtins
(set-face-foreground 'font-lock-builtin-face "Blue1")

;; Make diff mode colorful on the mac, and not psychotic on linux
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


;; Default "orange" color doesn't show up nicely if we don't have >8 colors, so
;; pick something that stands out more if we're color-impaired
(when (<= (length (list-colors-duplicates (defined-colors))) 8)
  (eval-after-load 'js2-mode
    '(progn
      (set-face-foreground 'js2-external-variable-face "white")
      (set-face-background 'js2-external-variable-face "red"))))

; Try to highlight "TODO: " entries
(defface todo-face
    '((t ()))
  "Face for highlighting comments like TODO: and HACK:")

(set-face-background 'todo-face cyan-name)

;; Add keywords we want highlighted
(defun add-todo-to-current-mode ()
  (font-lock-add-keywords nil
                          '(("\\(TODO\\|HACK\\|FIXME\\):" 1 'todo-face prepend))
                          t))

;; If we're highlighting beyond 80 characters, make it noticeable. The default
;; is to underline, which sucks.
;; TODO: use defface here
(set-face-background 'highlight-beyond-fill-column-face "#00FF00");;"#FFAFD7")
(set-face-underline-p 'highlight-beyond-fill-column-face nil)

;; WTF? I didn't ask for this
(eval-after-load 'python-mode
  '(progn
    (set-face-foreground 'py-XXX-tag-face 'unspecified)))

(provide 'custom-faces)
