(with-eval-after-load "isearch"
  (setq isearch-allow-scroll 'unlimited)
  (setq isearch-lazy-count t)
  (setq lazy-highlight-initial-delay 0)

  (define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)

  (when (require 'isearch-dabbrev nil t)
    (define-key isearch-mode-map (kbd "<tab>") 'isearch-dabbrev-expand)))
