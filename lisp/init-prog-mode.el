(defun font-lock-todo-setup ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|BUG\\)\\>" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'font-lock-todo-setup)

(defun indent-defun ()
  "Indent the current function.
See `indent-region'"
  (interactive "*")
  (save-excursion
    (let ((begin nil)
          (end nil))
      (beginning-of-defun)
      (setq begin (point))
      (end-of-defun)
      (setq end (point))
      (indent-region begin end))))

(defun smart-semicolon ()
  "Go to end of line, delete trailing whitespace and insert a \";\" unless
one already exists at point."
  (interactive "*")
  (move-end-of-line nil)
  (delete-horizontal-space)
  (unless (char-equal (preceding-char) (string-to-char ";"))
    (insert ";")))


;; key bindings
(define-key prog-mode-map (kbd "C-c C-q") 'indent-defun) ; similar to the c-indent-defun
(define-key prog-mode-map (kbd "C-;") 'smart-semicolon)
