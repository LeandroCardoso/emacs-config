(with-eval-after-load 'prog-mode
  (defun font-lock-todo-setup ()
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\|TODO\\|BUG\\)\\>" 1 font-lock-warning-face t))))
  
  (add-hook 'prog-mode-hook #'font-lock-todo-setup)

  (define-key prog-mode-map (kbd "C-;") 'smart-dot-comma)
  (define-key prog-mode-map (kbd "<f9>") 'compile))
