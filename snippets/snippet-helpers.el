;;; ruby-mode (chef)
(defun recipe-name ()
  "Assumes that the recipe file being edited is in a
properly-structured cookbook -- <recipe>/recipes/<recipe_file>.rb"
  (file-name-nondirectory (directory-file-name (locate-dominating-file default-directory "recipes"))))

(defun strip-leading-slash (text)
  "Strips the first leading '/' from a filename path.

TODO: This could be better done as a regex"
  (if (char-equal ?/ (string-to-char text))
      (substring text 1)
      text))
