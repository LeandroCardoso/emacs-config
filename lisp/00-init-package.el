(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa stable" . "http://stable.melpa.org/packages/") t)

(setq package-archive-priorities
      '(("melpa stable" . 2)
        ("gnu"          . 1)
        ("melpa"        . 0)))
