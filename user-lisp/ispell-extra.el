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

Word-list files must be available in `ispell-words-directory' and
must be named after the locale with a \".txt\" extension."
  (let* ((dict (or ispell-local-dictionary
                   ispell-dictionary))
         (locale (or (cadr (assoc dict ispell-dicts-name2locale-equivs-alist))
                     dict))
         (file (and locale
                    (expand-file-name (concat locale ".txt") ispell-words-directory)))
         (local (and ispell-local-dictionary
                     (not (equal ispell-local-dictionary
                                 ispell-dictionary)))))

    (if local
        (setq-local ispell-complete-word-dict file)
      (setq ispell-complete-word-dict file))

    (message "%s Ispell word-list dictionary set to %s"
             (if local "Local" "Global")
             (abbreviate-file-name file))

    (unless (file-exists-p file)
      (message "Warning: Ispell word-list dictionary %s does not exist"
               (abbreviate-file-name file)))))

;;;###autoload
(defun ispell-dictionary-info()
  "Display information about ispell dictionaries."
  (interactive)
  (message "ispell dictionaries, local: %s, default: %s, word-list: %s"
           ispell-local-dictionary
           ispell-dictionary
           (abbreviate-file-name ispell-complete-word-dict)))

(unless (eq system-type 'windows-nt)
  (add-hook 'ispell-change-dictionary-hook 'ispell-change-word-dict))

(provide 'ispell-extra)

;;; ispell-extra.el ends here
