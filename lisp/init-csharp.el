;; csharp
(when (require 'csharp-mode nil t)
  ;; omnisharp
  (when (require 'omnisharp nil t)
    (setq omnisharp-imenu-support t)
    (setq omnisharp-server-executable-path (concat user-emacs-directory "omnisharp/OmniSharp.exe"))
    
    (add-hook 'csharp-mode-hook #'omnisharp-mode)

    (with-eval-after-load "flycheck"
      (add-hook 'csharp-mode-hook #'flycheck-mode))

    ;; omnisharp + company
    (with-eval-after-load "company"
      (add-to-list 'company-backends 'company-omnisharp)
      ;; this value is slow (setq omnisharp-company-match-type 'company-match-server)
      )
    ))
