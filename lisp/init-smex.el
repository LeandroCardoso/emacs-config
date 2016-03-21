(with-eval-after-load "smex"
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  ;; keymap
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x") 'execute-extended-command) ;; old M-x
  (smex-initialize))
