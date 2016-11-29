(setq-default comment-column 0)

;; Some modes (like emacs-lisp-mode) have the bad habit of overwriting comment-column. This
;; workaround this behavior.
(add-hook 'prog-mode-hook (lambda () (setq comment-column (default-value 'comment-column))))
