(defgroup TSO6 nil
  "Major mode for editing TSO6 files"
  :group 'convenience)

(defvar tso6-mode-hook nil)

(defconst tso6-mode-line-help-echo
  ;; See bindings.el for details of `mode-line-format' construction.
  (get-text-property 0 'help-echo (car default-mode-line-format))
  "Primary default mode line help echo text.")

(defconst tso6-mode-line-format
  ;; See bindings.el for details of `mode-line-format' construction.
  (append (butlast default-mode-line-format 2)
	  (cons `(tso6-field-name-string
		  ("" tso6-field-name-string
		   ,(propertize "" 'help-echo tso6-mode-line-help-echo)))
		(last default-mode-line-format 2)))
  "Mode line format string for TSO6 mode.")

(defvar tso6-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for TSO6 major mode")

;;; autoload
(add-to-list 'auto-mode-alist '("\\.TSO6\\." . tso6-mode))

(defvar tso6-mode-syntax-table
  (let ((st (make-syntax-table)))
    st))

(defconst tso6-font-lock-keywords-1
  (list
   '()
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for TSO6 mode")

(defvar tso6-font-lock-keywords tso6-font-lock-keywords-1
  "Default highlighting expressions for TSO6 mode")


(defvar header-spec
  (list
    '(1 1 "Record Type (H)")
    '(2 9 "Record Date")
    '(10 15 "Record Time")
    '(16 26 "Member ICA")
    '(27 86 "File Name")
    '(87 201 "Filler")))

(defvar data-spec
  (list
    '(1 1 "Record Type (D)")
    '(2 31 "Bank Customer Number")
    '(32 50 "Bank Account Number")
    '(51 70 "Bank Product Code")
    '(71 92 "Transaction Description")
    '(93 105 "Rebate Amount")
    '(106 106 "Exception Reason Code")
    '(107 136 "Exception Reason Description")
    '(137 144 "Rebate File Sent Date")
    '(145 157 "Transaction Sequence Number")
    '(157 201 "Filler")))

(defvar trailer-spec
  (list
    '(1 1 "Record Type (T)")
    '(2 13 "Exception Record Count")
    '(14 25 "Success Record Count")
    '(26 37 "Total Processed Record Count")
    '(38 48 "Member ICA")
    '(49 201 "Filler")))


(defun first-spec-hit (spec pos)
  "Given a spec and a position, find and return the first hit.

Returns nil if no hit found"
  (dolist (spec-item spec)
    (let ((start (nth 0 spec-item))
          (end (nth 1 spec-item)))
      (when (and (<= start pos end))
        (let ((result (nth 2 spec-item)))
          (return result))))))

(defun line-type ()
  "Determines the record type of the current line"
  (interactive)
  (let* ((char (char-after (line-beginning-position)))
         (type (if char (char-to-string char)
                      nil)))
    type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Field name mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom tso6-field-name-delay 0.125
  "Time in seconds to delay before updating field name display."
  :group 'TSO6
  :type '(number :tag "seconds"))

(defvar tso6-field-name-idle-timer nil)

(defvar tso6-field-name-string nil)
(make-variable-buffer-local 'tso6-field-name-string)

(defvar tso6-field-name-string-old nil)
(make-variable-buffer-local 'tso6-field-name-string-old)

(define-minor-mode tso6-field-name-mode
  "Toggle TSO6-field-name mode.
When enabled, the name of the current field appears in the mode line."
  :group 'TSO6
  :global t
  :init-value t
  ;; First, always disable current timer to avoid having two timers.
  (when tso6-field-name-idle-timer
    (cancel-timer tso6-field-name-idle-timer)
    (setq tso6-field-name-idle-timer nil))
  ;; Now, if mode is on and any buffer is in TSO6 mode then re-initialize and
  ;; enable by setting up a new timer
  (if tso6-field-name-mode
      (if (memq t (mapcar (lambda (buffer)
                            (with-current-buffer buffer
                              (when (eq major-mode 'tso6-mode)
                                (setq tso6-field-name-string nil
                                      tso6-field-name-string-old nil)
                                t)))
                          (buffer-list)))
          (setq tso6-field-name-idle-timer
                (run-with-idle-timer tso6-field-name-delay t
                                     'tso6-field-name-display)))
    ;; but if the mode is off then remove the display from the mode lines of
    ;; all TSO6 buffers
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (eq major-mode 'tso6-mode)
                (setq tso6-field-name-string nil
                      tso6-field-name-string-old nil)
                (force-mode-line-update))))
          (buffer-list))))


(defun current-line-pos ()
  "Yields the current position within the line"
  (interactive)
  (+ 1 (- (point) (line-beginning-position))))

(defun current-field-name ()
  "Find the name of the field at the current position in the current line."
  (interactive)
  (let* ((type (line-type))
         (spec (cond ((string= type "H") header-spec)
                     ((string= type "D") data-spec)
                     ((string= type "T") trailer-spec)
                     (t nil))))
    (if spec
        ;; TODO: find a better way to find position within a line
        (let ((line-pos (current-line-pos)))
          (first-spec-hit spec line-pos)))))

(defun tso6-field-name-display ()
  "Construct `tso6-field-name-string' to display in mode line.
Called by `tso6-field-name-idle-timer'."
  (if (eq major-mode 'tso6-mode)
      (let ((field-name (current-field-name)))
        (when (not (string= field-name tso6-field-name-string-old))
          (setq tso6-field-name-string-old field-name
                tso6-field-name-string
                (and field-name (propertize (format "%s" field-name)
                                            'help-echo tso6-mode-line-help-echo)))
          (force-mode-line-update)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tso6-mode ()
  "Major mode for editing TSO6 files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table tso6-mode-syntax-table)
  (use-local-map tso6-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(tso6-font-lock-keywords))
  (setq major-mode 'tso6-mode
        mode-name "TSO6"
        mode-line-format tso6-mode-line-format)
  (run-hooks 'tso6-mode-hook))



(provide 'tso6-mode)
