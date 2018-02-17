(setq frame-resize-pixelwise t)
(setq highlight-nonselected-windows t)
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 5) ;; recenter if scroll more than value
(setq scroll-margin 1)
(setq scroll-preserve-screen-position t)
(setq sentence-end-double-space nil)
(setq truncate-partial-width-windows nil)
(setq undo-limit 800000)
(setq undo-strong-limit undo-limit)
(setq-default abbrev-mode t) ;; enable abbrev-mode by default
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq-default tab-width 4)

(modify-coding-system-alist 'file "\\.el\\'" 'prefer-utf-8-unix)

;; ESC key toogle the minibuffer
;; related commands: keyboard-escape-quit keyboard-quit minibuffer-keyboard-quit
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)


;; zap - misc.el
(autoload 'zap-up-to-char "misc")
(global-set-key (kbd "M-z") 'zap-up-to-char) ;; default is zap-to-char


;; novice.el
(setq disabled-command-function nil)


;; simple.el
(setq column-number-mode t)
(setq completion-show-help nil)
(setq kill-do-not-save-duplicates t)
(setq kill-whole-line t)
(setq next-error-highlight 'fringe-arrow)
(setq normal-erase-is-backspace nil)
(setq shift-select-mode nil)

(global-set-key (kbd "RET") 'newline-and-indent) ;; default is newline
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-M-|") 'delete-indentation)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-u") 'upcase-dwim) ;; default is upcase-word
(global-set-key (kbd "M-l") 'downcase-dwim) ;; default is downcase-word
(global-set-key (kbd "M-c") 'capitalize-dwim) ;; default is capitalize-word


;; startup.el
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)


;; subr.el
(defalias 'yes-or-no-p 'y-or-n-p)


;; indent.el
(setq tab-always-indent 'complete)


;; iso-transl
;; This is required to dead keys works properly in my linux
(require 'iso-transl)
