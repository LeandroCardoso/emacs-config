(when (require 'flycheck nil t)
  (setq flycheck-check-syntax-automatically (if (eq system-type 'windows-nt)
                                                '(save idle-change)
                                              '(save idle-change mode-enabled)))
  (setq flycheck-checker-error-threshold nil)
  (setq flycheck-completing-read-function 'ido-completing-read)
  (setq flycheck-global-modes '(c-mode c++-mode csharp-mode typescript-mode))
  (setq flycheck-idle-change-delay 5)
  (setq flycheck-mode-line-prefix "!")

  ;; Increased the "File" column size
  (seq-doseq (el flycheck-error-list-format)
    (when (string= (car el) "File")
      (setcdr el 20)))

  ;; cppcheck
  ;; unusedStructMember is annoying in header files
  (setq flycheck-cppcheck-suppressions '("unusedStructMember"))

  (global-flycheck-mode)
  )
