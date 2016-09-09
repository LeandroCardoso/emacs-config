(with-eval-after-load "isearch"
  (when (require 'isearch-dabbrev nil t)
    (define-key isearch-mode-map (kbd "<tab>") 'isearch-dabbrev-expand)))
