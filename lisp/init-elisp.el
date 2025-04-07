(require 'elisp-mode)
(require 'mode-local)
(setq-mode-local emacs-lisp-mode sentence-end-double-space t)

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun) ; eval-defun is also in C-M-x
