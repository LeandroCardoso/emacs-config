(with-eval-after-load "compile"
  (setq compilation-scroll-output 'first-error)
  (setq compilation-error-screen-columns nil))

(global-set-key (kbd "<f9>") 'compile)
