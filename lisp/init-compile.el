(with-eval-after-load "compile"
  (setq compilation-scroll-output 'first-error)
  (setq compilation-error-screen-columns nil))

(define-key prog-mode-map (kbd "<f9>") 'compile)
