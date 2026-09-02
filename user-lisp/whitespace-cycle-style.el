;;; whitespace-cycle-style.el --- Cycle whitespace styles for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:
;; Cycle between predefined `whitespace-style' configurations.

;;; Code:

(require 'whitespace)
(require 'seq)

(defcustom whitespace-cycle-styles
  (list `("Default" ,(eval (car (get 'whitespace-style 'standard-value))))
        '("Newline" (face newline-mark newline))
        '("Page delimiters" (face page-delimiters)))
  "List of `whitespace-style' configurations to cycle through.

Each element has the form:

    (NAME STYLE)

NAME is a string used to identify STYLE in messages and other UI.

STYLE is a value accepted by `whitespace-style'. Its structure and
available values are defined by the `whitespace-style' custom variable.

For example:

    ((\"Default\" (face trailing tabs spaces))
     (\"Newline\" (face newline-mark newline))
     (\"Page delimiters\" (face page-delimiters)))

The command `whitespace-cycle-style' cycles through these styles in the
order they appear in this list."
  :type `(repeat
          (list (string :tag "Name")
           ,(get 'whitespace-style 'custom-type)))
  :group 'whitespace)

(defun whitespace-cycle-style ()
  "Cycle `whitespace-style' through `whitespace-cycle-styles'.

Set `whitespace-style' to the next configured style specified by
`whitespace-cycle-styles', wrapping from the last style back to the
first."
  (interactive)
  (let* ((styles (mapcar #'cadr whitespace-cycle-styles))
         (pos (seq-position styles whitespace-active-style))
         (next (nth (if (and pos (< (1+ pos) (length styles)))
                        (1+ pos)
                      0)
                    whitespace-cycle-styles)))
    (setq whitespace-style (cadr next))
    (whitespace-mode -1)
    (whitespace-mode 1)
    (message "Whitespace style set to %s" (car next))))

(provide 'whitespace-cycle-style)

;;; whitespace-cycle-style.el ends here
