;;; msherry-c.el --- Util functions for C-like languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marc Sherry

;; Author: Marc Sherry <msherry@msherry-mbp.corp.dropbox.com>
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

(defun bh-choose-header-mode ()
  "Choose the correct C style (Objective-C, C++, C) when opening a .h file.

  Based on the presence of a similarly-named .m/.cpp file.

Based on
http://bretthutley.com/programming/emacs/opening-a-cobjective-cc-header-file-in-emacs/,
but with additional hacks for frameworks by Marc Sherry"
  (interactive)
  (let ((fn (buffer-file-name)))
    (if (string-equal (substring fn -2) ".h")
        (progn
          ;; OK, we got a .h file, if a .m file exists we'll assume it's an
          ;; objective c file. Otherwise, we'll look for a .cpp file.
          (let ((dot-m-file (concat (substring fn 0 -1) "m"))
                (dot-cpp-file (concat (substring fn 0 -1) "cpp")))
            (if (file-exists-p dot-m-file)
                (objc-mode)
                (if (file-exists-p dot-cpp-file)
                    (c++-mode))
                ;; Could be C, or could be Objective-C with no matching .m file
                ;; (e.g., framework headers). Check for the #import directive,
                ;; which is mostly Objective-C (and Microsoft-specific C++).
                (progn
                  (if (with-temp-buffer
                        (insert-file-contents fn)
                        (goto-char (point-min))
                        (re-search-forward "^#import\\|@\"\\|@protocol" nil t))
                      (objc-mode)))))))))
(add-hook 'find-file-hook 'bh-choose-header-mode)


(provide 'msherry-c)
;;; msherry-c.el ends here
