(when (require 'doom-modeline nil t)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-height 30)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-vcs-max-length 22)
  (doom-modeline-mode 1))