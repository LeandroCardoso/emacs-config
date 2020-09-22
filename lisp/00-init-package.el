(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa stable" . "https://stable.melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa"        . 2)
        ("melpa stable" . 1)
        ("gnu"          . 0)))

(global-set-key (kbd "C-x p") 'list-packages)
