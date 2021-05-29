;;; flycheck-arduino.el --- Arduino support for flycheck.

;; Authors: stardiviner <numbchild@gmail.com>
;; Version: 0.1
;; Keywords: arduino flycheck

;; flycheck-arduino is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; flycheck-arduino is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:



;;; Code:

(require 'flycheck)

(defvar flycheck-arduino-board nil
  "The Arduino board to be used for debugging Sketch.")

(flycheck-define-checker arduino
  "Arduino checker using Arduino IDE. (This requires higher than version 1.5+).
See `https://github.com/arduino/Arduino/blob/master/build/shared/manpage.adoc'."
  ;; source, source-inplace, source-original
  :command ("arduino" "--verify" source)
  ;; :command `("arduino-cli" "debug" "-b" ,flycheck-arduino-board ,(projectile-project-root))
  :error-patterns
  (;; I don't make sure about this warning... How to emit a warning?
   (warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
   (error   line-start (file-name) ":" line ":" column ": " (0+ "fatal ") "error: " (message) line-end))
  :modes (arduino-mode))

;;;###autoload
(defun flycheck-arduino-setup ()
  "Setup Flycheck Arduino.
Add `arduino' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'arduino))



(provide 'flycheck-arduino)

;;; flycheck-arduino.el ends here
