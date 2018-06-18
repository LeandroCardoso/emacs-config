(when (require 'rotate-text nil t)
  (setq rotate-text-words
        '(("width" "height")
          ("left" "right" "top" "bottom")
          ("true" "false")
          ("yes" "no")
          ("enable" "disable")
          ("enabled" "disabled")))

  (setq rotate-text-symbols
        '(("private" "protected" "public")
          ("nil" "t")))

  (global-set-key (kbd "C-=") 'rotate-text)
  (global-set-key (kbd "C-+") 'rotate-text-backward)
  )
