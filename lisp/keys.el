(global-set-key (kbd "C-M-o") 'ff-find-other-file) ;; default is split-line
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x M-o") 'other-frame)
(global-set-key (kbd "M-/") 'hippie-expand) ;; default is dabbrev-expand
(global-set-key (kbd "M-?") 'tags-search)
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; default is list-buffers
(global-set-key (kbd "C-x 4 C-b") 'ibuffer-other-window)
(global-set-key (kbd "RET") 'newline-and-indent) ;; default is newline
(global-set-key (kbd "M-RET") 'smart-newline-and-indent) ;; ALT ENTER
(global-set-key (kbd "C-M-<delete>") 'backward-kill-sexp)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c g") 'ag) ;; it used to be rgrep
(global-set-key (kbd "C-z") 'idomenu)
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)
(global-set-key (kbd "C-c -") 'shrink-window) ;; default is backward-page
(global-set-key (kbd "C-c +") 'enlarge-window) ;; default is forward-page
(global-set-key (kbd "C-x M-d") 'find-name-dired)
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-h") 'mark-line) ;; default is help prefix, but we have f1
(global-set-key (kbd "<f1> x") 'which-key-show-top-level)
(global-set-key (kbd "C-c m") 'woman)

;; unset compose-mail keys to use it with magit
(global-unset-key (kbd "C-x m")) ;; compose-mail
(global-unset-key (kbd "C-x 4 m")) ;; compose-mail-other-window
(global-unset-key (kbd "C-x 5 m")) ;; compose-mail-other-frame
;; magit
(defvar magit-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'magit-dispatch-popup)
    (define-key map "f" 'magit-file-popup)
    (define-key map "s" 'magit-status)
    (define-key map "i" 'magit-init)
    (define-key map "c" 'magit-clone)
    map)
  "keymap for global magit commands")
(defalias 'magit-global-keymap magit-global-keymap)
(global-set-key (kbd "C-x m") 'magit-global-keymap)


(global-set-key (kbd "C-c t") 'transpose-paragraphs)
;; FIX subword-mode-map remaping
;;(global-set-key (kbd "C-T") 'transpose-words) ;; original transpose-words is remaped to
                                                  ;; subword-transpose
(global-set-key (kbd "C-M-|") 'delete-indentation)
(global-set-key (kbd "C-;") 'smart-dot-comma)

;; scroll
(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

;; whitespace
(global-set-key (kbd "C-c w") 'whitespace-cleanup-region)
(global-set-key (kbd "C-c W") 'whitespace-cleanup)
(global-set-key (kbd "C-c M-w") 'whitespace-mode)
(global-set-key (kbd "C-c M-W") 'whitespace-newline-mode)

;; smex
(eval-after-load "smex"
  '(progn
     (global-set-key (kbd "M-x") 'smex)
     (global-set-key (kbd "M-X") 'smex-major-mode-commands)
     (global-set-key (kbd "C-c M-x") 'execute-extended-command))) ;; old M-x

;; ESC key toogle the minibuffer
;; related commands: keyboard-escape-quit keyboard-quit minibuffer-keyboard-quit
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort) ;; isearch
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)
(global-set-key (kbd "<escape>") 'smex) ;; everywhere else

;; prog-mode-maps seems to be bugged
;;(define-key prog-mode-map (kbd "<f9>") 'compile)
(global-set-key (kbd "<f9>") 'compile)

;; transpose-frame
(global-set-key (kbd "C-x |") 'rotate-frame-clockwise)
(global-set-key (kbd "C-x \\") 'rotate-frame)


;; mode specific maps

;; occur
(define-key occur-mode-map (kbd "<tab>") 'occur-next)
(define-key occur-mode-map (kbd "<backtab>") 'occur-prev)

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "C-c b") 'browse-url-of-dired-file)
     (define-key dired-mode-map (kbd "M-m") 'dired-move-to-filename-i)
     (define-key dired-mode-map (kbd "<tab>") 'dired-next-line)
     (define-key dired-mode-map (kbd "<backtab>") 'dired-previous-line)))

(eval-after-load "ediff-mult"
  '(add-hook 'ediff-meta-buffer-keymap-setup-hook
             (lambda ()
               (define-key ediff-meta-buffer-map (kbd "<tab>") 'ediff-next-meta-item)
               (define-key ediff-meta-buffer-map (kbd "<backtab>") 'ediff-previous-meta-item))))

(eval-after-load "wdired"
  '(define-key wdired-mode-map (kbd "M-m") 'dired-move-to-filename-i))

(eval-after-load "yasnippet"
  '(progn
     (define-key yas-minor-mode-map (kbd "<tab>") nil)
     (define-key yas-minor-mode-map (kbd "TAB") nil)
     (define-key yas-minor-mode-map (kbd "C-S-<tab>") 'yas-expand)
     ))

;; company
(eval-after-load "company"
  '(progn
     (define-key company-mode-map (kbd "<C-tab>") 'company-complete-common-or-cycle)
     ;;(define-key company-active-map (kbd "<tab>") 'company-select-next)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))
