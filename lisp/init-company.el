(when (require 'company nil t)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (make-local-variable 'company-backends)
              ;;(push '(company-semantic :with company-yasnippet company-keywords) company-backends)))
              (push '(company-dabbrev-code :with company-yasnippet company-keywords) company-backends)))

  ;; Enable company mode in all modes, except problematic ones.
  ;;(setq company-global-modes '(not ))
  
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 3)
  (setq company-search-regexp-function 'company-search-flex-regexp)
  (setq company-show-numbers t)
  (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
  
  ;; company dabbrev
  (setq company-dabbrev-downcase nil)

  ;; company dabbrev code
  (setq company-dabbrev-code-everywhere t)
  
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

  (global-company-mode)
  
  (when (require 'company-flx nil t)
    (company-flx-mode +1)))
