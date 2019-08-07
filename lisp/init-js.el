(when (require 'js2-mode nil t)
  ;; Use js2-minor-mode for syntax highlight, jump-to-definition and imenu

  (defun js2-minor-mode-setup ()
    (setq-local imenu-create-index-function #'js2-mode-create-imenu-index))

  (define-key js2-minor-mode-map [remap js-find-symbol] #'js2-jump-to-definition)

  (add-hook 'js2-minor-mode-hook 'js2-minor-mode-setup)
  (add-hook 'js-mode-hook 'js2-minor-mode)

  (setq js2-missing-semi-one-line-override t)
  (setq js2-mode-assume-strict t)
  (setq-default js2-idle-timer-delay 1))
