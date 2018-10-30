(setq eval-expression-print-length nil)
(setq frame-resize-pixelwise t)
(setq gc-cons-threshold 10000000) ; 10 MB
(setq highlight-nonselected-windows t)
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)
(setq undo-limit 800000)
(setq undo-strong-limit undo-limit)

;; novice.el
(setq disabled-command-function nil)

;; simple.el
(setq completion-show-help nil)
(setq kill-do-not-save-duplicates t)
(setq normal-erase-is-backspace nil)
(setq shift-select-mode nil)

;; startup.el
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

;; indent.el
(setq tab-always-indent 'complete)

;; subr.el
(defalias 'yes-or-no-p 'y-or-n-p)

;; iso-transl - This is required for dead keys work in linux
(require 'iso-transl)

;; Set coding system to utf-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Force unix EOL in emacs-lisp files outside of unix
(modify-coding-system-alist 'file "\\.el\\'" 'prefer-utf-8-unix)

;; ESC key toogle the minibuffer
;; related commands: keyboard-escape-quit keyboard-quit minibuffer-keyboard-quit
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

;; Hack to set the major mode automatically with new buffers not associated with a file
;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
(setq-default major-mode
              (lambda () (if buffer-file-name
                             (fundamental-mode)
                           (let ((buffer-file-name (buffer-name)))
                             (set-auto-mode)))))
