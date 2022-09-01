(with-eval-after-load "isearch"
  (setq isearch-allow-motion t)
  (setq isearch-lazy-count t)
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-initial-delay 0)

  (define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
  (define-key isearch-mode-map (kbd "M-y") 'isearch-yank-pop)

  (when (require 'isearch-dabbrev nil t)
    (define-key isearch-mode-map (kbd "<tab>") 'isearch-dabbrev-expand)))
