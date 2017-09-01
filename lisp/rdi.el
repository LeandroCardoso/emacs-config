(add-to-list 'auto-mode-alist '("\\.nps\\'" . javascript-mode))

;; Use TABs with XML files
(add-hook 'nxml-mode-hook (lambda () (setq indent-tabs-mode t)))
;; Use TABs with javascript files
(add-hook 'js-mode-hook (lambda () (setq indent-tabs-mode t)))

;; There are some c++ files using .c extension.
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; np6 log mode
(modify-coding-system-alist 'file "\\.log\\'" 'prefer-utf-8-dos)
(define-generic-mode np6-log-mode ;; MODE
  nil                             ;; COMMENT-LIST
  nil                             ;; KEYWORD-LIST
  '(("^\\<\\(INFO\\|DEBUG\\)\\>" . font-lock-function-name-face)
    ("^\\<WARNING\\>" . compilation-warning-face)
    ("^\\<\\(ERROR\\|FATAL\\)\\>" . compilation-error-face)
    ("\t.*\t" . font-lock-comment-face)
    )                             ;; FONT-LOCK-LIST
  '("[0-9]\\{8\\}\\(_DEBUG\\)?-[0-9]\\{3\\}\\.log$") ;; AUTO-MODE-LIST
  (list
   (lambda ()
     (setq global-auto-revert-ignore-buffer t))
   ) ;; FUNCTION-LIST
  )

;; Ugly hack to disable automatic string highlight. This is disabled due to several malformed
;; strings.
(require 'mode-local)
(setq-mode-local np6-log-mode font-lock-keywords-only t)

;; np6 production log mode
(define-generic-mode np6-prod-log-mode ;; MODE
  nil                             ;; COMMENT-LIST
  nil                             ;; KEYWORD-LIST
  '(("\\(INFO\\|FINEST\\|DEBUG\\)" . font-lock-function-name-face)
    ("WARNING" . compilation-warning-face)
    ("\\(ERROR\\|FATAL\\)" . compilation-error-face)
    ("^[0-9- :]*;" . font-lock-comment-face) ; timestamp
    ("\\[com.*\\] Thread: .*$" . font-lock-comment-face)
    )                             ;; FONT-LOCK-LIST
  '("\\(newposv6\\|np6[a-z]*\\)-0\\.0\\.log") ;; AUTO-MODE-LIST
  (list
   (lambda ()
     (setq global-auto-revert-ignore-buffer t))
   ) ;; FUNCTION-LIST
  )

;; .np6 and .npsharp mode
(define-generic-mode np6-mode ;; MODE
  '(";")                      ;; COMMENT-LIST
  '("|")                      ;; KEYWORD-LIST
  nil                         ;; FONT-LOCK-LIST
  '("\\.np6$" "\\.npstart$")  ;; AUTO-MODE-LIST
  nil                         ;; FUNCTION-LIST
  )
