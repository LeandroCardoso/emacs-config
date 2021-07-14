;; csharp
(with-eval-after-load "csharp-mode"
  (defun csharp-mode-setup ()
    (local-set-key (kbd "/") 'c-electric-slash))

  (add-hook 'csharp-mode-hook 'csharp-mode-setup)

  (add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))


  ;; omnisharp
  (when (require 'omnisharp nil t)
    ;; Workaround for dot net framework not found
    (when (eq system-type 'gnu/linux)
      (setenv "FrameworkPathOverride" "/lib/mono/4.5"))

    (defun omnisharp-restart-omnisharp-server (&optional no-autodetect)
      "Stops Omnisharp server if running and starts an OmniSharp server
for a given path to a project or solution file.

See `omnisharp-stop-server' and `omnisharp-start-omnisharp-server'."
      (interactive "P")
      (omnisharp-stop-server)
      (sleep-for 0.5)
      (omnisharp-start-omnisharp-server no-autodetect))


    (add-hook 'csharp-mode-hook 'omnisharp-mode)

    (setq omnisharp-cache-directory (expand-file-name "omnisharp" user-emacs-directory))
    (setq omnisharp-company-do-template-completion nil)

    ;; omnisharp + company
    (with-eval-after-load "company"
      (add-to-list 'company-backends 'company-omnisharp))

    (defun omnisharp-setup-hook ()
      ;; keymap
      (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
      (define-key omnisharp-mode-map (kbd "M-?") 'omnisharp-find-usages)
      (define-key omnisharp-mode-map (kbd "C-x 4 .") 'omnisharp-go-to-definition-other-window)
      (define-key omnisharp-mode-map (kbd "C-M-z") 'omnisharp-navigate-to-solution-member)
      (define-key omnisharp-mode-map (kbd "C-c C-o") 'omnisharp-restart-omnisharp-server))

    (add-hook 'omnisharp-mode-hook #'omnisharp-setup-hook)))
