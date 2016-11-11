(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; goto-addr does not have a key-binding map variable, so I am using the prog-mode-map.
(define-key prog-mode-map (kbd "C-c RET") 'goto-address-at-point)
