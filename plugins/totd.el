(require 'cl)

(defun totd ()
  "Tip of the day.

Print out a random emacs function and documentation.
Found at http://emacs.wordpress.com/2007/06/21/tip-of-the-day/, then modified"
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n"
               "========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))


(defvar jao-totd-timer (run-at-time "12:00pm" (* 3600 24) 'totd))

(defun jao-cancel-totd
  (interactive)
  (cancel-timer jao-totd-timer))

(provide 'totd)
