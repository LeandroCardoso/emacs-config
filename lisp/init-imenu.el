(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 100000)
(setq imenu-max-item-length nil)

;; idomenu
(when (require 'idomenu nil t)
  (global-set-key (kbd "C-z") 'idomenu)) ; default is suspend-frame
