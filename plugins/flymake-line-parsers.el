;; parse Iskren's imojslint output into something flymake can use
(defvar imojslint-err-line-pattern; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
  '("\\(^[0-9]+\\)|##|\\([0-9]+\\)|##|\\([^|]+\\)|##|\\([^|]+\\)|##|\\([^|]+\\)"
    3 1 2 5 4))

;; Modified version of this function - allows a 5th element in patterns to
;; specify whether a line is a warning. Used only for Iskren's imojslist script
(defun imo-flymake-parse-line (line)
  "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise."
  (let ((raw-file-name nil)
	(line-no 0)
	(err-type "e")
	(err-text nil)
	(patterns flymake-err-line-patterns)
	(matched nil))
    (while (and patterns (not matched))
      (when (string-match (car (car patterns)) line)
	(let* ((file-idx (nth 1 (car patterns)))
	       (line-idx (nth 2 (car patterns))))

	  (setq raw-file-name (if file-idx (match-string file-idx line) nil))
	  (setq line-no       (if line-idx (string-to-number (match-string line-idx line)) 0))
	  (setq err-text      (if (> (length (car patterns)) 4)
				  (match-string (nth 4 (car patterns)) line)
				(flymake-patch-err-text (substring line (match-end 0)))))
	  (or err-text (setq err-text "<no error text>"))
          ;; This first part is the part we changed
	  (if (or (and (> (length (car patterns)) 4)
                       (string-match "^[wW].*"
                                     (match-string (nth 5 (car patterns)) line)))
                  (and err-text (string-match "^[wW]arning" err-text)))
                     (setq err-type "w")
              )
	  (flymake-log 3 "parse line: file-idx=%s line-idx=%s file=%s line=%s text=%s" file-idx line-idx
		       raw-file-name line-no err-text)
	  (setq matched t)))
      (setq patterns (cdr patterns)))
    (if matched
	(flymake-ler-make-ler raw-file-name line-no err-type err-text)
      ())))

(defun use-imo-flymake-fun ()
  "Use the 5-argument flymake-parse-line, which is required for
Iskren's jslint script"
  (setq use-hacked-flymake-parse-line 1))


(provide 'flymake-line-parsers)
