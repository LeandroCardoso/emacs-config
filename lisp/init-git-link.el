(when (require 'git-link nil t)
  (setq git-link-use-commit t)

  ;; keys
  (defvar git-link-keymap nil "Keymap for git-link commands")
  (setq git-link-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map "l" 'git-link)
          (define-key map "c" 'git-link-commit)
          (define-key map "h" 'git-link-homepage)
          map))
  (defalias 'git-link-keymap git-link-keymap)
  (global-set-key (kbd "C-c l") 'git-link-keymap))
