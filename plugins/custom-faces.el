;; Marc Sherry

;; Make diff mode colorful on the mac, and not psychotic on linux
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


;; Default yellow color doesn't show up nicely on linux, so pick something that
;; stands out more
(eval-after-load 'js2-mode
  '(progn
    (set-face-foreground 'js2-external-variable-face "white")
    (set-face-background 'js2-external-variable-face "red")))

; Try to highlight "TODO: " entries
(defface todo-face
    '((t ("cyan")))
  "Face for highlighting comments like TODO: and HACK:")

(set-face-background 'todo-face "cyan")

;; Add keywords we want highlighted
(defun add-todo-to-current-mode ()
  (font-lock-add-keywords nil
                          '(("\\(TODO\\|HACK\\|FIXME\\):" 1 'todo-face prepend))
                          t))

(provide 'custom-faces)
