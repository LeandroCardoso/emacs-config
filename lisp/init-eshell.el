(require 'em-alias)
;; Changing from the default value (~/.emacs.d/eshell/alias) because the eshell directory is in the
;; .gitignore, so I can ignore the transient files inside this directory.
(setq eshell-aliases-file (concat user-emacs-directory "eshell_alias"))

(require 'em-hist)
(setq eshell-hist-ignoredups t)

(require 'em-term)
(add-to-list 'eshell-visual-commands "watch")

(defun eshell-other-window (&optional arg)
  "Like `eshell', but creates a new window or reuses an existing
one."
  (interactive "P")
  (if (one-window-p)
      (split-window)
    (other-window 1))
  (eshell arg))

(global-set-key (kbd "C-x C-$") 'eshell)
(global-set-key (kbd "C-x 4 C-$") 'eshell-other-window)
