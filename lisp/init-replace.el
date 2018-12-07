;; occur
(add-hook 'occur-mode-hook #'(lambda () (setq truncate-lines t)))

(define-key occur-mode-map (kbd "<tab>") 'occur-next)
(define-key occur-mode-map (kbd "<backtab>") 'occur-prev)

(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

(define-key occur-mode-map (kbd "k") 'keep-lines)
(define-key occur-mode-map (kbd "f") 'flush-lines)
