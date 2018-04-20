(with-eval-after-load 'prog-mode
  (defun font-lock-todo-setup ()
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\|TODO\\|BUG\\)\\>" 1 font-lock-warning-face t))))
  
  (add-hook 'prog-mode-hook #'font-lock-todo-setup))


;; functions

(defun smart-dot-comma ()
  "Go to end of line and insert a \";\""
  (interactive "*")
  (move-end-of-line nil)
  (insert ";"))

;; key bindings

(define-key prog-mode-map (kbd "C-;") 'smart-dot-comma)
(define-key prog-mode-map (kbd "<f9>") 'compile)
