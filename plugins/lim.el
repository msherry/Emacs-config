;;; lim.el --- Lim it

;;; Commentary: Cloned from https://github.com/progfolio/lim


;;; Code:
;;;;Requirements
(eval-when-compile (require 'subr-x))

;;;;Declarations
(declare-function org-back-to-heading   "org")
(declare-function org-entry-get         "org")
(declare-function org-set-property      "org")
(declare-function org-narrow-to-subtree "org")

(defvar org-heading-regexp)
(defvar org-drawer-regexp)
(defvar org-keyword-regexp)
(defvar lim-it 375
  "Current region character limit.")

;;@INCOMPLETE:
;;This shouldn't need to be a backquoted list.
;;@OPTIMIZATION:
;;we should concatenate all the regexps into a single expression
;;to overhead while checking against them.
;;This necessitates a way to reload the regexp instead of setting it.
(defvar lim-ignored-expressions `("[[:space:]]"
                                  ,org-heading-regexp
                                  ,org-drawer-regexp
                                  ;; ,org-keyword-regexp
                                  ,org-property-re)
  "List of regular expressions to ignore in char count.")

(defun lim-acknowledged-char-p ()
  "Return t if char is does not satisfy `lim-ignored-expressions'."
  (not (seq-some #'looking-at-p lim-ignored-expressions)))

(defun lim-seek ()
  "Find next character."
  (save-match-data
    (while (seq-some #'looking-at lim-ignored-expressions)
      (ignore-errors (forward-char (length (match-string 0)))))))

(defun lim-seek-ignored ()
  "Seek to next ignored character."
  (while (and (not (eobp)) (lim-acknowledged-char-p))
    (ignore-errors (forward-char))))

(defun lim-count-chars ()
  "Count characters in current region spare `lim-ignored-expressions'."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((count 0))
        (while (not (eobp))
          (lim-seek)
          (unless (eobp)
            (setq count (1+ count))
            (ignore-errors (forward-char))))
        count))))

(defun lim-update-header-line ()
  "Update header line with character usage info."
  (let* ((char-count (lim-count-chars))
         (percentage (* 100 (/ char-count (float lim-it)))))
    (setq header-line-format
          (concat (format "LIM: %03d/%03d %.2f"
                          char-count
                          lim-it
                          percentage)
                  "%%"))))

(defun lim-add-faces ()
  "Add faces."
  (save-excursion
    (goto-char (point-min))
    (let ((remaining lim-it))
      (lim-seek)
      (unless (eobp)
        (setq remaining (1- remaining)))
      (while (not (eobp))
        (if (<= remaining 0)
            (let ((start (point)))
              (lim-seek-ignored)
              (add-text-properties start (point) '(face lim-over-limit font-lock-face lim-over-limit))
              (lim-seek))
          (ignore-errors (forward-char))
          (while (and (> remaining 0)
                      (not (eobp))
                      (lim-acknowledged-char-p))
            (setq remaining (1- remaining))
            (ignore-errors (forward-char))
            (lim-seek)))))))

(defvar-local lim-current-buffer nil
  "Current buffer lim is running in.
Used to make sure debounced functions execute in proper buffer.")

(defun lim-apply-limit (&optional _beg _end _length)
  "Apply the face at the limit."
  ;;inhibit undo recording while applying faces
  (let ((buffer-undo-list t))
  (when (eq (current-buffer) lim-current-buffer)
    (let ((pmin (point-min))
          (pmax (point-max)))
      (when (> pmax (+ pmin lim-it))
        (remove-list-of-text-properties pmin pmax '(face font-lock-face))
        (lim-add-faces)
        (font-lock-flush)
        (font-lock-ensure)
        (funcall lim-update-info-function))))))

(defun lim-apply-limit-maybe ()
  "Apply limit for commands in `lim-after-commands'."
  (when (member this-command lim-after-commands)
    (lim-apply-limit)
    (lim-update-header-line)))

(defvar lim-after-change-delay 0.25
  "Number of seconds to delay before applying `lim-over-limit-face'.")

(defvar-local lim-input-timer nil
  "Timer for debounced face application.")

(defun lim-display-org-heading-info ()
  "Display lim info at end of org mode heading."
  ;;assumes narrow to heading
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let* ((char-count (lim-count-chars))
             (percentage (* 100 (/ char-count (float lim-it))))
             (info (concat (format " LIM: %03d/%03d %.2f"
                                   char-count
                                   lim-it
                                   percentage)
                           "%%"))
             ;;@INCOMPLETE: need to filter just for our overlay
             (overlay (or (car (overlays-at (point)))
                          (make-overlay (point) (re-search-forward org-heading-regexp)))))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'after-string (propertize info
                                                       'face (if (> percentage 100)
                                                                 'lim-info-over
                                                               'lim-info)
                                                       'font-lock-face (if (> percentage 100)
                                                                           'lim-info-over
                                                                         'lim-info)))))))
(defun lim-org-heading--get-set-limit ()
  "Return or prompt for the heading limit."
  (if-let ((l (org-entry-get (point) "lim")))
      (string-to-number l)
    (let ((limit (read-number "Org Entry LIM: ")))
      (org-set-property "lim" (number-to-string limit))
      limit)))

(defun lim-org-entry ()
  "Narrow to heading.
Optionally set limit."
  (interactive)
  (org-back-to-heading)
  (org-narrow-to-subtree)
  (setq lim-update-info-function 'lim-display-org-heading-info)
  (setq header-line-format nil)
  (setq lim-it (lim-org-heading--get-set-limit)))


(defun lim-apply-limit-deferred (&optional _beg _end _length)
  "Debounced version of `lim-apply-limit'."
  ;;@refactor: this should be moved somewhere else.
  (funcall lim-update-info-function)
  (when lim-input-timer (cancel-timer lim-input-timer))
  (setq lim-input-timer (run-with-timer lim-after-change-delay nil
                                        #'lim-apply-limit)))

;;;###autoload
(define-minor-mode lim-mode
  "Lim it"
  :lighter " lim"
  :keymap nil
  (cond (lim-mode
         (add-hook 'after-change-functions #'lim-apply-limit-deferred nil t)
         (add-hook 'post-command-hook #'lim-apply-limit-maybe nil t)
         ;;this needs to happen before applying limit
         (setq lim-current-buffer (current-buffer))
         (lim-apply-limit)
         (funcall lim-update-info-function)
         (make-local-variable 'lim-it))
        (t
         (remove-hook 'after-change-functions #'lim-apply-limit-deferred t)
         (remove-hook 'post-command-hook #'lim-apply-limit-maybe t)
         (remove-list-of-text-properties (point-min) (point-max) '(face font-lock-face))
         (setq header-line-format nil)
         (font-lock-flush))))

;;;;Commands
(defun lim-set (&optional n)
  "Set `limi-it' for current region to N chars.
If N is nil, default to `lim-it'."
  (interactive "nLim-it:")
  (setq lim-it (abs (floor n)))
  (lim-apply-limit))

(provide 'lim)

;;; lim.el ends here
