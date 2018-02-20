;;; Code:

(require 'go-mode)

(add-hook 'go-mode-hook
          '(lambda ()
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c C-g") 'go-goto-imports)
            (local-set-key (kbd "C-c C-f") 'gofmt)
            (local-set-key (kbd "C-c C-k") 'godoc)
            (local-set-key (kbd "M-.") 'godef-jump)))

(add-hook 'before-save-hook 'gofmt-before-save)


(provide 'msherry-go)

;;; msherry-go ends here
