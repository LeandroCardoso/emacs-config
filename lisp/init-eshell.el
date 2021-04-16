(require 'em-alias)
;; Changing from the default value (~/.emacs.d/eshell/alias) because the eshell directory is in the
;; .gitignore, so I can ignore the transient files inside this directory.
(setq eshell-aliases-file (concat user-emacs-directory "eshell_alias"))

(require 'em-term)
(add-to-list 'eshell-visual-commands "watch")

(global-set-key (kbd "C-x C-$") 'eshell)
