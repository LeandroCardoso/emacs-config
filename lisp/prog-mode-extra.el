;;; prog-mode-extra.el --- Extra prog-mode commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Fontify \"BUG\", \"FIXME\" and \"TODO\" words:
;;; (add-hook 'prog-mode-hook 'font-lock-todo-setup)

;;; Commands indent-defun and smart-semicolon can be bind to keys:
;; (define-key prog-mode-map (kbd "C-c C-q") 'indent-defun)
;; (define-key prog-mode-map (kbd "C-;") 'smart-semicolon)

;;; Code:

(defun font-lock-todo-setup ()
  "Fontify \"BUG\", \"FIXME\" and \"TODO\" keywords."
  (require 'font-lock)
  (font-lock-add-keywords
   nil
   '(("\\<\\(BUG\\|FIXME\\|TODO\\)\\>" 1 font-lock-warning-face t))))

;;;###autoload
(defun smart-semicolon ()
  "Go to end of line, delete trailing whitespace and insert a \";\".

A \";\" is not inserted when one already exists at end of line."
  (interactive "*")
  (require 'simple)
  (move-end-of-line nil)
  (delete-horizontal-space)
  (unless (char-equal (preceding-char) (string-to-char ";"))
    (insert ";")))

(provide 'prog-mode-extra)

;;; prog-mode-extra.el ends here
