(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2)
                            (c-set-offset 'arglist)))

(provide 'java-settings)
