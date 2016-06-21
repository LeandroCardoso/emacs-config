(when (require 'company nil t)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (make-local-variable 'company-backends)
              ;;(push '(company-semantic :with company-yasnippet company-keywords) company-backends)))
              (push '(company-dabbrev-code :with company-yasnippet company-keywords) company-backends)))
  (add-hook 'nxml-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (push '(company-nxml company-dabbrev company-yasnippet) company-backends)))

  ;; Enable company mode in all prog-mode and text-mode derived modes.
  (add-hook 'prog-mode-hook 'company-mode-on)
  (add-hook 'text-mode-hook 'company-mode-on)

  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
  
  ;; company dabrev
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  ;; (setq company-dabbrev-time-limit 0.05)

  ;; company dabbre code
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-ignore-case t)
  ;; (setq company-dabbrev-code-time-limit 0.05)
  
  ;; TODO add c++ keywords to company-keywords-alist
  ;; (alignas alignof char16_t char32_t constexpr decltype noexcept nullptr static_assert thread_local)

  ;; keymap
  (define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
  (define-key company-mode-map (kbd "<C-tab>") 'company-complete)
  ;; (define-key company-mode-map (kbd "<C-tab>") 'company-complete-common-or-cycle)
  
  ;; (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))