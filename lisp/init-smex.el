(when (require 'smex nil t)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  
  ;; keymap
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "<escape>") 'smex)
  (global-set-key (kbd "<S-escape>") 'smex-major-mode-commands)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-x M-x") 'execute-extended-command) ;; old M-x
  (smex-initialize))
