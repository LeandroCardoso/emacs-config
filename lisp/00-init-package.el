(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa stable" . "https://stable.melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa"        . 3)
        ("nongnu"       . 2)
        ("gnu"          . 1)
        ("melpa stable" . 0)))

(define-key ctl-x-map (kbd "C-p") 'list-packages) ; original is mark-page
