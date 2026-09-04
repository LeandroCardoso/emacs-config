;;; ispell-extra.el --- Extra ispell commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'ispell)

(defcustom ispell-words-directory nil
  "Directory where ispell word-list dictionaries reside.

See `ispell-complete-word-dict' and `ispell-change-word-dict'."
  :type 'directory
  :group 'ispell)

(defun ispell-change-word-dict ()
  "Change the word-list dictionary used for word completion.

Word-list files must be available in the `ispell-words-directory' and
must be named with the locale and a \"txt\" extenstion."
  (let* ((locale (car (alist-get (or ispell-local-dictionary ispell-dictionary)
                                 ispell-dicts-name2locale-equivs-alist nil nil 'equal)))
         (file (when locale
                 (expand-file-name (concat locale ".txt") ispell-words-directory)))
         (local (and ispell-local-dictionary
                     (not (eq ispell-local-dictionary ispell-dictionary)))))
    (if (eq system-type 'windows-nt)
        (message "Ispell word-list dictionary disabled in Windows")
      (if local
          (setq-local ispell-complete-word-dict file)
        (setq ispell-complete-word-dict file))
      (message "%s Ispell word-list dictionary set to %s"
               (if local "Local" "Global")
               file)
      (when (not (file-exists-p file))
        (message "Warning: Ispell word-list dictorary %s does not exist" file)))))

;;;###autoload
(defun ispell-dictionary-info()
  "Display information about ispell dictionaries."
  (interactive)
  (message "ispell local dictionary: %s, default dictionary: %s, word-list dictionary: %s"
           ispell-local-dictionary ispell-dictionary ispell-complete-word-dict))

(add-hook 'ispell-change-dictionary-hook 'ispell-change-word-dict)

(provide 'ispell-extra)

;;; ispell-extra.el ends here
