;;; python-mode
(defun prev-def-name ()
  (save-excursion
    (if (re-search-backward "def +\\(.+?\\)(" nil t)
    (match-string 1))))

(defun prev-def-args ()
  (save-excursion
    (if (re-search-backward "def +\\(.+?\\)( *self *,? *\\(.*\\))" nil t)
    (match-string 2))))

(defun prev-class-name ()
  (save-excursion
    (if (re-search-backward "class +\\(.+?\\) *[(:]" nil t)
    (match-string 1))))

;;; ruby-mode (chef)
(defun recipe-name ()
  "Assumes that the recipe file being edited is in a
properly-structured cookbook -- <recipe>/recipes/<recipe_file>.rb"
  (file-name-nondirectory (directory-file-name (locate-dominating-file default-directory "recipes"))))

(defun strip-leading-slash (text)
  (if (char-equal ?/ (string-to-char text))
      (substring text 1)
      text))
