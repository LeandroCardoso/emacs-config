(when (require 'company nil t)
  (setq company-format-margin-function 'company-text-icons-margin)

  ;; dabbrev
  (setq company-dabbrev-char-regexp "\\sw\\|_\\|-")
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-buffers nil)

  ;; dabbrev code
  (setq company-dabbrev-code-everywhere t)

  ;; keymap
  (define-key prog-mode-map (kbd "C-<tab>") 'company-indent-or-complete-common)
  (define-key text-mode-map (kbd "C-<tab>") 'company-indent-or-complete-common)

  ;; company-active-map
  (define-key company-active-map (kbd "<escape>") 'company-abort)
  (define-key company-active-map (kbd "<next>") 'company-next-page)
  (define-key company-active-map (kbd "C-v") 'company-next-page)
  (define-key company-active-map (kbd "<prior>") 'company-previous-page)
  (define-key company-active-map (kbd "M-v") 'company-previous-page)

  ;; company-search-map
  (define-key company-search-map (kbd "<escape>") 'company-search-abort)

  (company-tng-mode)
  (global-company-mode)

  (when (require 'company-flx nil t)
    (company-flx-mode +1))

  (when (require 'company-c-headers nil t)
    (add-to-list 'company-backends 'company-c-headers)))
