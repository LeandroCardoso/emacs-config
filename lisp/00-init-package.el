(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa stable" . "http://stable.melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa"        . 2)
        ("melpa stable" . 1)
        ("gnu"          . 0)))

(global-set-key (kbd "C-x p") 'list-packages)
