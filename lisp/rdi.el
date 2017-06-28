(add-to-list 'auto-mode-alist '("\\.nps\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("start\\.np" . bat-mode))

;; Use TABs with XML files
(add-hook 'nxml-mode-hook (lambda () (setq indent-tabs-mode t)))
