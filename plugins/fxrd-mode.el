(defgroup FXRD nil
  "Major mode for editing fixed field width files"
  :group 'convenience)

(defface fxrd-current-field-face
  '((t (:inherit highlight
        :background "pink")))
  "Highlight the current field."
  :group 'FXRD)
(defvar fxrd-current-field-face 'fxrd-current-field-face)

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
;;; Fxrd file specifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tso6-spec
  '(("H" (
          (1 1 "Record Type (H)")
          (2 9 "Record Date")
          (10 15 "Record Time")
          (16 26 "Member ICA")
          (27 86 "File Name")
          (87 201 "Filler")))
    ("D" (
          (1 1 "Record Type (D)")
          (2 31 "Bank Customer Number")
          (32 50 "Bank Account Number")
          (51 70 "Bank Product Code")
          (71 92 "Transaction Description")
          (93 105 "Rebate Amount")
          (106 106 "Exception Reason Code")
          (107 136 "Exception Reason Description")
          (137 144 "Rebate File Sent Date")
          (145 157 "Transaction Sequence Number")
          (157 201 "Filler")))
    ("T" (
          (1 1 "Record Type (T)")
          (2 13 "Exception Record Count")
          (14 25 "Success Record Count")
          (26 37 "Total Processed Record Count")
          (38 48 "Member ICA")
          (49 201 "Filler")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-line-pos ()
  "Yields the current position within the line"
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

(defun get-name-from-spec-item (spec-item)
  "Given a spec item, extract the name part."
  (nth 2 spec-item))

(defun line-type ()
  "Determines the record type of the current line"
  (let* ((char (char-after (line-beginning-position)))
         (type (if char (char-to-string char))))
    type))

(defun current-field-name ()
  "Find the name of the field at the current position in the current line."
  (let ((record-spec (get-spec-for-line)))
    (if record-spec
        ;; TODO: find a better way to find position within a line
        (let ((line-pos (current-line-pos)))
          (get-name-from-spec-item (first-spec-hit record-spec line-pos))))))

(defun current-field-boundaries ()
  "Find the (absolute) start and end position of the field at the current position."
  (let ((record-spec (get-spec-for-line)))
    (if record-spec
        ;; TODO: find a better way to find position within a line
        (let ((line-pos (current-line-pos)))
          (let* ((line-start (line-beginning-position))
                 (spec-item (first-spec-hit record-spec line-pos))
                 (start (1- (+ line-start (nth 0 spec-item))))
                 (end (+ line-start (nth 1 spec-item))))
            (list start end))))))

(defun fxrd-clear-overlays ()
  (remove-overlays nil nil 'fxrd-overlay t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun next-field ()
  "Move to the start of the next field."
  (interactive)
  (let* ((field-boundaries (current-field-boundaries))
         (next-field-start (nth 1 field-boundaries)))
    (goto-char (min next-field-start (point-max)))))

(defun previous-field ()
  "Move to the start of the previous field."
  (interactive)
  (let* ((field-boundaries (current-field-boundaries))
         (prev-field-end (1- (nth 0 field-boundaries))))
    (goto-char (max prev-field-end (point-min)))
    (let* ((prev-field-boundaries (current-field-boundaries))
           (begin (nth 0 prev-field-boundaries)))
      (goto-char begin))))


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
                                     'fxrd-field-name-display)))
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

(defun fxrd-field-name-display ()
  "Construct `fxrd-field-name-string' to display in mode line.
Called by `fxrd-field-name-idle-timer'."
  (if (derived-mode-p 'fxrd-mode)
      (let ((field-name (current-field-name))
            (field-boundaries (current-field-boundaries)))
        (when (not (string= field-name fxrd-field-name-string-old))
          ;; Update modeline
          (setq fxrd-field-name-string-old field-name
                fxrd-field-name-string
                (and field-name (propertize (format "%s" field-name)
                                            'help-echo fxrd-mode-line-help-echo)))
          (force-mode-line-update)
          ;; Highlight current field
          (remove-overlays nil nil 'fxrd-overlay t)
          (let* ((line-start (line-beginning-position))
                 (begin (nth 0 field-boundaries))
                 (end (nth 1 field-boundaries))
                 (overlay (make-overlay begin end)))
            (overlay-put overlay 'fxrd-overlay t)
            (overlay-put overlay 'face fxrd-current-field-face))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode fxrd-mode nil "FXRD"
  "Major mode for editing fixed field width files.

\\{fxrd-mode-map}"
  :group 'fxrd
  :syntax-table fxrd-mode-syntax-table
  (use-local-map fxrd-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(fxrd-font-lock-keywords))
  (fxrd-field-name-mode 1)
  (overwrite-mode)
  (add-hook (make-local-variable 'change-major-mode-hook) 'disable-fxrd-mode))

;;;###autoload
(define-derived-mode tso6-mode fxrd-mode "TSO6"
  "Major mode for editing TSO6 fixed field width files.

\\{fxrd-mode-map}"
  (setq fxrd-current-spec tso6-spec))

(provide 'fxrd-mode)
