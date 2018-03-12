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

  ;; c/c++ mode
  (defun c-common-set-company ()
    (setq-local company-backends
                '((company-etags company-dabbrev-code company-yasnippet company-keywords)
                  (company-dabbrev-code company-yasnippet company-keywords))))

  (add-hook 'c-mode-common-hook 'c-common-set-company)

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

  (with-eval-after-load "cc-mode"
    (define-key c-mode-base-map (kbd "<tab>") 'company-indent-or-complete-common)
    (define-key c-mode-base-map (kbd "<C-tab>") 'company-semantic))
  
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "<C-tab>") 'company-complete))

  (with-eval-after-load "shell"
    (define-key shell-mode-map (kbd "<C-tab>") 'company-complete))

  (define-key company-active-map (kbd "<escape>") 'company-abort)
  (define-key company-active-map (kbd "<next>") 'company-next-page)
  (define-key company-active-map (kbd "C-v") 'company-next-page)
  (define-key company-active-map (kbd "<prior>") 'company-previous-page)
  (define-key company-active-map (kbd "M-v") 'company-previous-page)
  (define-key company-active-map (kbd "<C-tab>") 'company-complete-common)

  (define-key company-search-map (kbd "<escape>") 'company-search-abort)


  (global-company-mode)

  (when (require 'company-flx nil t)
    (company-flx-mode +1)))
