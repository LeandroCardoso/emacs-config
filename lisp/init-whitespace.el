(when (require 'whitespace nil t)
  (setq whitespace-line-column nil)     ; use `fill-column'

  (defvar whitespace-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map "c" 'whitespace-cleanup)
      (define-key map "n" 'whitespace-newline-mode)
      (define-key map "o" 'whitespace-toggle-options)
      (define-key map "r" 'delete-whitespace-rectangle) ; rect.el
      (define-key map "t" 'delete-trailing-whitespace) ; simple.el
      (define-key map "w" 'whitespace-mode)
      map)
    "Keymap for whitespace commands")

  (defalias 'whitespace-keymap whitespace-keymap)
  (global-set-key (kbd "C-c w") 'whitespace-keymap))
