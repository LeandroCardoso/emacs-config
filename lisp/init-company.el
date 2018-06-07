(when (require 'company nil t)
  (setq company-idle-delay 0.3)
  (setq company-lighter-base "comp")
  (setq company-minimum-prefix-length 3)
  (setq company-search-regexp-function 'company-search-flex-regexp)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-margin 2)
  (setq company-tooltip-minimum-width 32)
  (setq company-transformers '(company-sort-by-backend-importance))

  (company-tng-configure-default)

  ;; dabbrev
  (setq company-dabbrev-downcase nil)

  ;; dabbrev code
  (setq company-dabbrev-code-everywhere t)

  ;; etags
  (setq company-etags-everywhere t)

  ;; gtags - In Windows company-gtags-executable is set with the full path to global executable -
  ;; this is great, but does not work. Set it to just "global" fix it.
  (when (eq system-type 'windows-nt)
    (setq company-gtags-executable "global"))

  ;; keymap
  (define-key prog-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
  (define-key text-mode-map (kbd "<tab>") 'company-indent-or-complete-common)

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "<C-tab>") 'company-complete))

  (with-eval-after-load "shell"
    (define-key shell-mode-map (kbd "<C-tab>") 'company-complete))

  (define-key company-active-map (kbd "<escape>") 'company-abort)
  (define-key company-active-map (kbd "<next>") 'company-next-page)
  (define-key company-active-map (kbd "C-v") 'company-next-page)
  (define-key company-active-map (kbd "<prior>") 'company-previous-page)
  (define-key company-active-map (kbd "M-v") 'company-previous-page)
  (define-key company-active-map (kbd "C-c <tab>") 'company-complete-common)
  (define-key company-active-map (kbd "<S-tab>") 'company-select-previous) ; workaround for tng in org-mode

  (define-key company-search-map (kbd "<escape>") 'company-search-abort)

  (global-company-mode)

  (when (require 'company-flx nil t)
    (company-flx-mode +1)))
