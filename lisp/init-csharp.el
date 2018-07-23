;; csharp
(with-eval-after-load "csharp-mode"
  ;; omnisharp
  (when (require 'omnisharp nil t)
    (defun omnisharp-smart-start-server (&optional no-autodetect)
      "Stops Omnisharp server if running and starts an OmniSharp server for a given path to a project or solution file.

See `omnisharp-stop-server' and `omnisharp-start-omnisharp-server'."
      (interactive "P")
      (omnisharp-stop-server)
      (sleep-for 0.5)
      (omnisharp-start-omnisharp-server no-autodetect))


    (add-hook 'csharp-mode-hook 'omnisharp-mode)

    (setq omnisharp-imenu-support nil) ; disabled because it does not work when server is not running
    (setq omnisharp-server-executable-path (concat user-emacs-directory "omnisharp/OmniSharp.exe"))

    ;; omnisharp + company
    (with-eval-after-load "company"
      (add-to-list 'company-backends 'company-omnisharp))

    (defun omnisharp-setup-hook ()
      ;; keymap
      (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-find-implementations)
      (define-key omnisharp-mode-map (kbd "M-?") 'omnisharp-find-usages)
      (define-key omnisharp-mode-map (kbd "C-x 4 .") 'omnisharp-go-to-definition-other-window)
      ; TODO fallback to imenu if omnisharp is not running
      (define-key omnisharp-mode-map (kbd "C-z") 'omnisharp-navigate-to-current-file-member) ; replaces imenu
      (define-key omnisharp-mode-map (kbd "C-M-z") 'omnisharp-navigate-to-solution-member)
      (define-key omnisharp-mode-map (kbd "C-c C-o") 'omnisharp-smart-start-server))

    (add-hook 'omnisharp-mode-hook #'omnisharp-setup-hook)))
