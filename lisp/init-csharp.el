;; csharp
(when (require 'csharp-mode nil t)
  ;; omnisharp
  (when (require 'omnisharp nil t)
    (setq omnisharp-server-executable-path (concat user-emacs-directory "omnisharp/OmniSharp.exe"))
    (add-hook 'csharp-mode-hook 'omnisharp-mode)

    ;; omnisharp + company
    (with-eval-after-load "company"
      (add-to-list 'company-backends 'company-omnisharp)
      ;; this value is currently bugged (setq omnisharp-company-match-type 'company-match-flx)
      ;; this value is slow (setq omnisharp-company-match-type 'company-match-server)
      (setq omnisharp-imenu-support t))
    )
  )
