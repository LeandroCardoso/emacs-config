(when (require 'flycheck nil t)
  (setq flycheck-check-syntax-automatically (if (eq system-type 'windows-nt)
                                                '(save idle-change idle-buffer-switch)
                                              '(save idle-change mode-enabled)))
  (setq flycheck-checker-error-threshold nil)
  (setq flycheck-completing-read-function 'ido-completing-read)
  (setq flycheck-global-modes '(c-mode c++-mode csharp-mode js-mode typescript-mode))
  (setq flycheck-idle-buffer-switch-delay (if (eq system-type 'windows-nt) 30 5))
  (setq flycheck-idle-change-delay (if (eq system-type 'windows-nt) 30 5))
  (setq flycheck-mode-line-prefix "!")

  ;; clang
  (flycheck-add-next-checker 'c/c++-clang 'c/c++-cppcheck)

  ;; cppcheck
  ;; unusedStructMember is annoying in header files
  (setq flycheck-cppcheck-suppressions '("unusedStructMember"))

  (global-flycheck-mode))
