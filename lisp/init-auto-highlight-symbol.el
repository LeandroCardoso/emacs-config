(when (require 'auto-highlight-symbol nil t)
  (setq ahs-idle-interval 0.5)

  ;; set ahs-inhibit-face-list
  (add-to-list 'ahs-inhibit-face-list 'font-lock-keyword-face)
  (add-to-list 'ahs-inhibit-face-list 'font-lock-type-face)

  ;; set ahs-modes
  (add-to-list 'ahs-modes 'js2-mode)

  (global-auto-highlight-symbol-mode))
