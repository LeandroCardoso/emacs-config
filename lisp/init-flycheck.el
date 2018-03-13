(when (require 'flycheck nil t)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-checker-error-threshold nil)
  (setq flycheck-completing-read-function 'ido-completing-read)
  (setq flycheck-global-modes '(c++-mode csharp-mode typescript-mode))
  (setq flycheck-idle-change-delay 5)
  (setq flycheck-mode-line-prefix nil)
  ;; TODO customize flycheck-mode-line. See flycheck-mode-line-status-text

  ;; cppcheck
  ;; unusedStructMember is annoying in header files
  (setq flycheck-cppcheck-suppressions '("unusedStructMember"))

  (global-flycheck-mode)
  )
