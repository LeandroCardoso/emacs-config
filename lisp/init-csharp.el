;; csharp
(with-eval-after-load "csharp-mode"
  (defun csharp-mode-setup ()
    (local-set-key (kbd "/") 'c-electric-slash))

  (add-hook 'csharp-mode-hook 'csharp-mode-setup)

  (add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode)))
