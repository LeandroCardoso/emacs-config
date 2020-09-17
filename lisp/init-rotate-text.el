(when (require 'rotate-text nil t)
  (setq rotate-text-words
        '(("width" "height")
          ("left" "right" "top" "bottom")
          ("enable" "disable")
          ("enabled" "disabled")
          ("true" "false")
          ("yes" "no")))

  (setq rotate-text-symbols
        '(("private" "protected" "public")
          ("on" "off")
          ("t" "nil")))

  (global-set-key (kbd "C-=") 'rotate-text)
  (global-set-key (kbd "C-+") 'rotate-text-backward)
  )
