(when (require 'rotate-text nil t)
  (setq rotate-text-words
        '(("height" "width")
          ("left" "right" "top" "bottom")
          ("active" "inactive")
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
