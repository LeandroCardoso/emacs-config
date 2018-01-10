(when (require 'company nil t)
  (defun c-common-set-company ()
    (setq-local company-backends
                '((company-etags company-dabbrev-code company-yasnippet company-keywords)
                  (company-dabbrev-code company-yasnippet company-keywords))))
  
  (add-hook 'c-mode-common-hook 'c-common-set-company)

  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 3)
  (setq company-search-regexp-function 'company-search-flex-regexp)
  (setq company-show-numbers t)
  (setq company-transformers '(company-sort-by-backend-importance))

  ;; company dabbrev
  (setq company-dabbrev-downcase nil)

  ;; company dabbrev code
  (setq company-dabbrev-code-everywhere t)

  ;; company etags
  (setq company-etags-everywhere t)

  ;; TODO add c++ keywords to company-keywords-alist
  ;; (alignas alignof char16_t char32_t constexpr decltype noexcept nullptr static_assert thread_local)

  ;; company-ispell
  (defun toggle-company-ispell ()
    (interactive)
    (cond
     ((memq 'company-ispell company-backends)
      (setq company-backends (delete 'company-ispell company-backends))
      (message "company-ispell disabled"))
     (t
      (add-to-list 'company-backends 'company-ispell)
      (message "company-ispell enabled!"))))

  ;; keymap
  (define-key prog-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
  (define-key text-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
  (define-key company-mode-map (kbd "<C-tab>") 'company-complete)
  
  (define-key company-search-map (kbd "<escape>") 'company-search-abort)
  (define-key company-active-map (kbd "<escape>") 'company-abort)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "<S-tab>") 'company-select-previous)

  (global-company-mode)
  
  (when (require 'company-flx nil t)
    (company-flx-mode +1)))
