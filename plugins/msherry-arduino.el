;;; msherry-arduino.el --- Miscellaneous utils/customizations for arduino development  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Marc Sherry

;; Author: Marc Sherry <marcsherry@Marc.S-MBPro>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun msherry/get-serial-buffer ()
  "Find an active serial buffer."
  (car
   (seq-filter
    #'(lambda (buf)
        (and
         (string-equal "/dev/"
                       (ignore-errors (substring (buffer-name buf) 0 5)))
         (string-equal "term-mode"
                       (with-current-buffer buf major-mode))))
    (buffer-list))))


(defun msherry/with-suspended-serial-process (orig &rest args)
  "Advice function for arduino-cli-mode.

If there is an active serial buffer, suspend it, run the advised
command, and restart the serial process."
  (let ((serial-buf (msherry/get-serial-buffer)))
    (if serial-buf
        (let ((window (get-buffer-window serial-buf))
              (serial-proc (get-buffer-process serial-buf)))
          (if serial-proc
              (let ((serial-args (process-contact serial-proc t)))
                (kill-buffer serial-buf)
                (let ((compilation-buffer (apply orig args)))
                  (with-current-buffer compilation-buffer
                    (add-hook
                     (make-local-variable 'compilation-finish-functions)
                     #'(lambda (buf msg)
                         ;; Give the arduino time to boot, to avoid crashing
                         ;; the host
                         (sleep-for 2)
                         (let* ((process (apply #'make-serial-process serial-args))
                                (buffer (process-buffer process)))
                           ;; Copied from serial-term
                           (with-current-buffer buffer
                             (term-mode)
                             (term-char-mode)
                             (goto-char (point-max))
                             (set-marker (process-mark process) (point))
                             (set-process-filter process #'term-emulate-terminal)
                             (set-process-sentinel process #'term-sentinel))
                           (set-window-buffer window buffer)))))
                  compilation-buffer))
              (progn
                ;; Buffer exists, but process already exited
                (kill-buffer serial-buf)
                (apply orig args))))
        ;; no serial buffer, call original func
        (apply orig args))))

(with-eval-after-load 'arduino-cli-mode
  (advice-add #'arduino-cli-compile-and-upload
              :around #'msherry/with-suspended-serial-process))



(provide 'msherry-arduino)
;;; msherry-arduino.el ends here
