(when (require 'whitespace nil t)
  (setq whitespace-line-column nil)     ; use `fill-column'
  (defvar whitespace-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map "w" 'whitespace-mode)
      (define-key map "n" 'whitespace-newline-mode)
      (define-key map "W" 'whitespace-toggle-options)
      (define-key map "c" 'whitespace-cleanup)
      (define-key map "t" 'delete-trailing-whitespace) ; simple.el
      (define-key map "r" 'delete-whitespace-rectangle) ; rect.el
      map)
    "Keymap for whitespace commands")
  (defalias 'whitespace-keymap whitespace-keymap)
  (global-set-key (kbd "C-c w") 'whitespace-keymap)
  )
