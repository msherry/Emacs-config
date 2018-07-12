;; Python-related settings and functions

;;; Use python-mode, instead of the crappy built-in python.el on the mac
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(autoload 'jedi-setup-venv "jedi-local" nil t)
(autoload 'jedi:setup "jedi" nil t)

(add-hook 'python-mode-hook
          '(lambda ()
            (setq jedi:setup-keys t)
            (jedi-setup-venv)
            (jedi:setup)
            (setq jedi:complete-on-dot t)
            (setq jedi:tooltip-method nil)
            (define-key jedi-mode-map (kbd "C-c .") nil)
            (elpy-mode)
            (define-key elpy-mode-map (kbd "<M-left>") nil)
            (define-key elpy-mode-map (kbd "<M-right>") nil)
            ;; (local-set-key (kbd "C-c .") 'flymake-goto-next-error)
            ))


;;; Advice
(defadvice py-shift-region-left (around keep-region-active
                                        (start end &optional count) activate)
  "Keep the region active so we can do multiple shifts."
  (let ((deactivate-mark nil))
    ad-do-it))
(defadvice py-shift-region-right (around keep-region-active
                                         (start end &optional count) activate)
  "Keep the region active so we can do multiple shifts."
  (let ((deactivate-mark nil))
    ad-do-it))
(defun dont-display-if-visible (orig-fun &rest args)
  "Don't switch to the elpy Python buffer if it's already visible."
  (if (not (get-buffer-window (process-buffer (elpy-shell-get-or-create-process))))
      (apply orig-fun args)))
(advice-add 'elpy-shell-display-buffer :around #'dont-display-if-visible)


(provide 'msherry-python)
