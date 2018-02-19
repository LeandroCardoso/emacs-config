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
  (company-tng-configure-default)

  ;; company dabbrev
  (setq company-dabbrev-downcase nil)

  ;; company dabbrev code
  (setq company-dabbrev-code-everywhere t)

  ;; company etags
  (setq company-etags-everywhere t)

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

  (define-key company-search-map (kbd "<escape>") 'company-search-abort)
  (define-key company-active-map (kbd "<escape>") 'company-abort)

  (global-company-mode)

  (when (require 'company-flx nil t)
    (company-flx-mode +1)))
