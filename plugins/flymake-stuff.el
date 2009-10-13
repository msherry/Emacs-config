;; Marc Sherry

;; Custom modes for flymake syntax checking

; On-the-fly pyflakes checking
(defvar python-check-command (if (eq system-type 'darwin)
                                 "pyflakes-2.5" ; dumb
                                 "pyflakes"))
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list python-check-command (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(defvar flymake-modes '(python-mode c-mode-common))

(mapcar '(lambda (x)
          (let ((mode-hook (intern (concat (symbol-name x) "-hook"))))
            (add-hook mode-hook
                      '(lambda ()
                        (flymake-mode 1)))))
        flymake-modes)

;; Prepend a different java handler, since flymake defaults to using Makefiles
;; TODO: this doesn't actually work - running flymake on java hangs emacs
(push '("\\.java\\'" flymake-simple-ant-java-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)

(provide 'flymake-stuff)
