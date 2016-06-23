(require 'go-mode)

(add-hook 'go-mode-hook
          '(lambda ()
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

(add-hook 'go-mode-hook
          '(lambda ()
            (local-set-key (kbd "C-c C-g") 'go-goto-imports)))

(add-hook 'go-mode-hook
          '(lambda ()
            (local-set-key (kbd "C-c C-f") 'gofmt)))
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-k") 'godoc)))


(provide 'go-stuff)
