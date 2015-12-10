;;; fxrd-mode.el --- Major mode for editing fixed field width files

;; Copyright (C) 2015 Marc Sherry

;; Author: Marc Sherry (msherry@gmail.com)
;; URL: https://github.com/msherry/fxrd-mode


;;; Commentary:

;; This package implements `fxrd-mode', a major mode for editing files with fixed
;; field widths. These files are commonly used in the financial industry, such
;; as in ACH transactions. This package provides:

;; - `nacha-mode': a mode for editing NACHA (ACH transaction) files
;; - `rm37-mode': a mode for editing RM37 (Mastercard rebate transaction) files
;; - `tso6-mode': a mode for editing TSO6 (Mastercard rebate confirmation) files

;; In each of these modes, the following commands are available:

;; - M-<right> (`next-field') and M-<left> (`previous-field') move to the next
;;   and previous fields, respectively.

;; This mode also provides validation of fields -- valid fields are highlighted
;; with `fxrd-current-field-face', while invalid fields are highlighted with
;; `fxrd-invalid-field-face' (both customizable). The current field name is
;; also displayed in the modeline.

;;; Installation:

;; Installation via MELPA is easiest.

(defgroup fxrd nil
  "Major mode for editing fixed field width files"
  :group 'convenience)

(defface fxrd-current-field-face
  '((t (:inherit highlight
        :background "pink")))
  "Highlight the current field."
  :group 'FXRD)
(defvar fxrd-current-field-face 'fxrd-current-field-face)

(defface fxrd-invalid-field-face
  '((t (:inherit highlight
        :background "red")))
  "Face for fields failing validation."
  :group 'FXRD)
(defvar fxrd-invalid-field-face 'fxrd-invalid-field-face)

(defconst fxrd-mode-line-help-echo
  ;; See bindings.el for details of `mode-line-format' construction.
  (get-text-property 0 'help-echo (car default-mode-line-format))
  "Primary default mode line help echo text.")

(defconst fxrd-mode-line-format
  ;; See bindings.el for details of `mode-line-format' construction.
  (append (butlast default-mode-line-format 2)
	  (cons `(fxrd-field-name-string
		  ("" fxrd-field-name-string
		   ,(propertize "" 'help-echo fxrd-mode-line-help-echo)))
		(last default-mode-line-format 2)))
  "Mode line format string for FXRD mode.")

(defconst fxrd-font-lock-keywords-1
  (list
   '()
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for FXRD mode")

(defvar fxrd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<right>") 'next-field)
    (define-key map (kbd "M-<left>") 'previous-field)
    map)
  "Keymap for FXRD major mode")

(defvar fxrd-mode-syntax-table
  (let ((st (make-syntax-table)))
    st))

(defvar fxrd-font-lock-keywords fxrd-font-lock-keywords-1
  "Default highlighting expressions for FXRD mode")

(defvar fxrd-current-spec nil)
(make-variable-buffer-local 'fxrd-current-spec)

(defvar fxrd-mode-hook nil)

(defun disable-fxrd-mode ()
  (fxrd-field-name-mode -1)
  (fxrd-clear-overlays))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'fxrd-validators)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-line-pos ()
  "Yields the current position within the line"
  ;; TODO: find a better way to find position within a line
  (+ 1 (- (point) (line-beginning-position))))

(defun get-spec-for-line ()
  "Finds the correct record spec to use for the current line, based on the first character."
  (when fxrd-current-spec
    (let* ((type (line-type))
           (record-spec (cadr (assoc type fxrd-current-spec))))
      record-spec)))

(defun first-spec-hit (record-spec pos)
  "Given a record spec and a position, find and return the first spec-item hit.

Returns nil if no hit found"
  (dolist (spec-item record-spec)
    (let ((start (nth 0 spec-item))
          (end (nth 1 spec-item)))
      (when (and (<= start pos end))
        (return spec-item)))))

(defun get-name-from-field-spec (field-spec)
  "Given a field spec, extract the name part."
  (nth 2 field-spec))

(defun get-validator-from-field-spec (field-spec)
  "Given a field spec, extract the validator, if present"
  (nth 3 field-spec))

(defun line-type ()
  "Determines the record type of the current line"
  (let* ((char (char-after (line-beginning-position)))
         (type (if char (char-to-string char))))
    type))

(defun get-current-field-spec ()
  (let ((record-spec (get-spec-for-line)))
    (if record-spec
        (let ((field-spec (first-spec-hit record-spec (current-line-pos))))
          field-spec))))

(defun current-field-name ()
  "Find the name of the field at the current position in the current line."
  (let ((field-spec (get-current-field-spec)))
    (when field-spec
      (get-name-from-field-spec field-spec))))

(defun current-field-boundaries ()
  "Find the (absolute) [start, end + 1] position of the field at
the current position.

`end' will actually be one more than the final position of the
field, due to the way most elisp functions (make-overlay,
buffer-substring, etc.) handle ranges."
  (let ((field-spec (get-current-field-spec)))
    (when field-spec
      (let* ((line-start (line-beginning-position))
             (start (1- (+ line-start (nth 0 field-spec))))
             (end (+ line-start (nth 1 field-spec))))
        (list start end)))))

(defun current-field-value ()
  "Find the contents of the current field"
  (let* ((field-boundaries (current-field-boundaries))
         (start (nth 0 field-boundaries))
         (end (nth 1 field-boundaries)))
    (when (and start end)
      (buffer-substring start end))))

(defun current-field-valid-p ()
  (let* ((field-spec (get-current-field-spec))
         (validator (get-validator-from-field-spec field-spec))
         (value (current-field-value)))
    (when validator
      (cond ((fxrd-validator-child-p validator)
             (fxrd-validate validator value))
            ((functionp validator) (funcall validator value))
            (t nil)))))

(defun fxrd-clear-overlays ()
  (remove-overlays nil nil 'fxrd-current-overlay t)
  (remove-overlays nil nil 'fxrd-invalid-overlay t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun next-field ()
  "Move to the start of the next field."
  (interactive)
  (let ((field-boundaries (or (current-field-boundaries)
                              (list (1+ (point)) (1+ (point))))))
    (when field-boundaries
      (let ((next-field-start (nth 1 field-boundaries)))
        (goto-char (min next-field-start (point-max)))))))

(defun previous-field ()
  "Move to the start of the previous field."
  (interactive)
  (let ((field-boundaries (current-field-boundaries)))
    (when field-boundaries
      (let ((prev-field-end (1- (nth 0 field-boundaries))))
        (goto-char (max prev-field-end (point-min)))
        (let ((prev-field-boundaries (current-field-boundaries)))
          (when prev-field-boundaries
            (let ((begin (nth 0 prev-field-boundaries)))
              (goto-char begin))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Field name mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom fxrd-field-name-delay 0.125
  "Time in seconds to delay before updating field name display."
  :group 'FXRD
  :type '(number :tag "seconds"))

(defvar fxrd-field-name-idle-timer nil)

(defvar fxrd-field-name-string nil)
(make-variable-buffer-local 'fxrd-field-name-string)

(defvar fxrd-field-name-string-old nil)
(make-variable-buffer-local 'fxrd-field-name-string-old)

(defvar fxrd-field-value-old
  "The last computed value of the current field"
  nil)
(make-variable-buffer-local 'fxrd-field-value-old)

(defvar fxrd-point-old
  "The last point"
  nil)
(make-variable-buffer-local 'fxrd-point-old)

(define-minor-mode fxrd-field-name-mode
  "Toggle FXRD-field-name mode.
When enabled, the name of the current field appears in the mode line."
  :group 'FXRD
  :global t
  :init-value t
  ;; First, always disable current timer to avoid having two timers.
  (when fxrd-field-name-idle-timer
    (cancel-timer fxrd-field-name-idle-timer)
    (setq fxrd-field-name-idle-timer nil))
  ;; Now, if mode is on and any buffer is in FXRD mode then re-initialize and
  ;; enable by setting up a new timer
  (if fxrd-field-name-mode
      (if (memq t (mapcar (lambda (buffer)
                            (with-current-buffer buffer
                              (when (derived-mode-p'fxrd-mode)
                                (setq fxrd-field-name-string nil
                                      fxrd-field-name-string-old nil)
                                t)))
                          (buffer-list)))
          (setq fxrd-field-name-idle-timer
                (run-with-idle-timer fxrd-field-name-delay t
                                     'fxrd-update-current-field)))
    ;; but if the mode is off then remove the display from the mode lines of
    ;; all FXRD buffers
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (derived-mode-p 'fxrd-mode)
                (setq fxrd-field-name-string nil
                      fxrd-field-name-string-old nil)
                (force-mode-line-update)
                (fxrd-clear-overlays))))
          (buffer-list))))

(defun fxrd-update-current-field ()
  "Construct `fxrd-field-name-string' to display in mode line.
Called by `fxrd-field-name-idle-timer'."
  (when (derived-mode-p 'fxrd-mode)
    (let ((field-name (current-field-name))
          (field-boundaries (current-field-boundaries))
          (field-value (current-field-value)))
      (when (not (string= field-name fxrd-field-name-string-old))
        ;; Update modeline
        (setq fxrd-field-name-string-old field-name
              fxrd-field-name-string
              (and field-name (propertize (format "%s" field-name)
                                          'help-echo fxrd-mode-line-help-echo)))
        (force-mode-line-update))
      ;; Highlight current field
      (if field-boundaries
          (when (not (string= fxrd-field-value-old
                              field-value))
            (setq fxrd-field-value-old field-value)
            (remove-overlays nil nil 'fxrd-current-overlay t)
            (let* ((begin (nth 0 field-boundaries))
                   (end (nth 1 field-boundaries))
                   (overlay (make-overlay begin end)))
              (overlay-put overlay 'fxrd-current-overlay t)
              (overlay-put overlay 'face
                           (cond ((current-field-valid-p) fxrd-current-field-face)
                                 (t fxrd-invalid-field-face)))))
        ;; Not in a field, clear the overlay
        (progn
          (setq fxrd-field-value-old nil)
          (remove-overlays nil nil 'fxrd-current-overlay t))))
    (fxrd-highlight-invalid-fields)))

(defun fxrd-highlight-invalid-fields ()
  "Highlight all invalid fields (except current field)"
  (when (not (eq fxrd-point-old (point)))
      (setq fxrd-point-old (point))
      (let ((cur-pos (point)))
        (save-mark-and-excursion
         (goto-char (point-min))
         (remove-overlays nil nil 'fxrd-invalid-overlay t)
         (let ((done nil)
               (last-pos (point)))
           (while (not done)
             (let ((field-boundaries (current-field-boundaries)))
               (when field-boundaries
                 (let ((begin (nth 0 field-boundaries))
                       (end (nth 1 field-boundaries)))
                   ;; Skip current field, it will be handled
                   ;; elsewhere. Remember to account for `end' being off by one
                   (when (and (not (<= begin cur-pos (1- end)))
                              (not (current-field-valid-p)))
                     (let ((overlay (make-overlay begin end)))
                       (overlay-put overlay 'fxrd-invalid-overlay t)
                       (overlay-put overlay 'face fxrd-invalid-field-face))))))
             (next-field)
             (if (eq (point) last-pos)
                 (setq done t))
             (setq last-pos (point))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode fxrd-mode nil "FXRD"
  "Major mode for editing fixed field width files.

\\{fxrd-mode-map}"
  :group 'fxrd
  :syntax-table fxrd-mode-syntax-table
  (use-local-map fxrd-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(fxrd-font-lock-keywords))
  (set (make-local-variable 'mode-line-format) fxrd-mode-line-format)
  (set (make-local-variable 'show-trailing-whitespace) nil)
  (fxrd-field-name-mode 1)
  (overwrite-mode)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook (make-local-variable 'change-major-mode-hook) 'disable-fxrd-mode))

(provide 'fxrd-mode)
;;; fxrd-mode.el ends here
