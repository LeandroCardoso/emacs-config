(setq package-enable-at-startup nil)
(package-initialize)

;; Workaround for emacs 26.1. See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (eq system-type 'windows-nt)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa stable" . "https://stable.melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa"        . 2)
        ("melpa stable" . 1)
        ("gnu"          . 0)))

(global-set-key (kbd "C-x p") 'list-packages)
