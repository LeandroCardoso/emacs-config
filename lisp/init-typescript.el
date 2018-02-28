(when (require 'tide nil t)
  (setq tide-completion-detailed t)
  (setq tide-hl-identifier-idle-time 0.1)
  (setq tide-imenu-flatten t)
  (setq tide-server-max-response-length 10240000)

  (defun tide-setup-hook ()
    (tide-setup)
    (eldoc-mode)
    (tide-hl-identifier-mode))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'tide-setup-hook)

  ;; TODO add support to lint

  ;; Force bundled tide tsserver
  (defun tide-force-bundled-tsserver ()
    "Force use of bundled tide tsserver.
See `tide-tsserver-executable'."
    (interactive)
    (setq tide-tsserver-executable
          (car (directory-files-recursively (concat user-emacs-directory "elpa/") tide--tsserver))))
  )
