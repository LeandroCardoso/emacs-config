(add-to-list 'auto-mode-alist '("\\.nps\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("start\\.np" . bat-mode))

;; Use TABs with XML files
(add-hook 'nxml-mode-hook (lambda () (setq indent-tabs-mode t)))
;; Use TABs with javascript files
(add-hook 'js-mode-hook (lambda () (setq indent-tabs-mode t)))

;; There are some c++ files using .c extension.
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; log-file-mode
(define-generic-mode
  'log-file-mode   ;; MODE
  nil              ;; COMMENT-LIST
  nil              ;; KEYWORD-LIST
  '(("^\\<\\(INFO\\|DEBUG\\)\\>" . 'font-lock-type-face)
    ("^\\<WARNING\\>" . compilation-warning-face)
    ("^\\<\\(ERROR\\|FATAL\\)\\>" . compilation-error-face)
    )              ;; FONT-LOCK-LIST
  '("\\.log$")     ;; AUTO-MODE-LIST
  nil              ;; FUNCTION-LIST
    )
