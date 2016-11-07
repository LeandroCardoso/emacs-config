(global-set-key (kbd "M-/") 'hippie-expand) ;; default is dabbrev-expand
(global-set-key (kbd "M-RET") 'smart-newline-and-indent) ;; ALT ENTER
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-h") 'mark-line) ;; default is help prefix, but we have f1

(global-set-key (kbd "C-c t") 'transpose-paragraphs)
;; FIX subword-mode-map remaping
;;(global-set-key (kbd "C-T") 'transpose-words) ;; original transpose-words is remaped to
                                                  ;; subword-transpose
(global-set-key (kbd "C-;") 'smart-dot-comma)

;; ESC key toogle the minibuffer
;; related commands: keyboard-escape-quit keyboard-quit minibuffer-keyboard-quit
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

;; prog-mode-maps seems to be bugged
;;(define-key prog-mode-map (kbd "<f9>") 'compile)
(global-set-key (kbd "<f9>") 'compile)
