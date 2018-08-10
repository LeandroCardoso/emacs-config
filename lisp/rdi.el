(when (string= (getenv "USERDOMAIN") "RDISOFTWARE")
  (require 'project)
  (add-to-list 'project-root-list "~/Documents/bugs/")
  (add-to-list 'project-root-list "~/Documents/env/")

  (if (featurep 'js2-mode)
      (progn
        (defun nps-setup-hook ()
          (when (string-match-p "\\.nps\\'" buffer-file-name)
            (setq-local js2-include-browser-externs nil)
            (setq-local js2-language-version 180)
            (push "API" js2-additional-externs)))

        (add-hook 'js2-mode-hook #'nps-setup-hook)
        (add-hook 'js2-minor-mode-hook #'nps-setup-hook)
        ;; (add-to-list 'auto-mode-alist '("\\.nps\\'" . js2-mode))
        ))

  (add-to-list 'auto-mode-alist '("\\.nps\\'" . js-mode))

  ;; Use TABs with XML, javascript and c/c++ files
  (add-hook 'nxml-mode-hook (lambda () (setq indent-tabs-mode t)))
  (add-hook 'js-mode-hook (lambda () (setq indent-tabs-mode t)))
  (add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t)))

  ;; There are some c++ files using .c extension.
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-or-c++-mode))

  (modify-coding-system-alist 'file "\\.log\\'" 'prefer-utf-8-dos)
  
  ;; np6 log mode
  (define-generic-mode np6-log-mode ;; MODE
    nil                             ;; COMMENT-LIST
    nil                             ;; KEYWORD-LIST
    '(("^\\<\\(INFO\\|DEBUG\\)\\>" . font-lock-function-name-face)
      ("^\\<WARNING\\>" . compilation-warning-face)
      ("^\\<\\(ERROR\\|FATAL\\)\\>" . compilation-error-face)
      ("\t.*\t" . font-lock-comment-face)
      )                                              ;; FONT-LOCK-LIST
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
    nil                                  ;; COMMENT-LIST
    nil                                  ;; KEYWORD-LIST
    '(("\\(INFO\\|FINEST\\|DEBUG\\)" . font-lock-function-name-face)
      ("WARNING" . compilation-warning-face)
      ("\\(ERROR\\|FATAL\\)" . compilation-error-face)
      ("^[0-9- :]*;" . font-lock-comment-face) ; timestamp
      ("\\[com.*\\] Thread: .*$" . font-lock-comment-face)
      )                                       ;; FONT-LOCK-LIST
    '("\\(newposv6\\|np6[a-z]*\\)-[0-9]\\.[0-9]\\.log$") ;; AUTO-MODE-LIST
    (list
     (lambda ()
       (setq global-auto-revert-ignore-buffer t))
     ) ;; FUNCTION-LIST
    )

  ;; np6 kiosk log mode
  (define-generic-mode np6-kiosk-log-mode ;; MODE
    nil                                  ;; COMMENT-LIST
    nil                                  ;; KEYWORD-LIST
    '((" \\(INFO\\|DEBUG\\) " . font-lock-function-name-face)
      (" WARN " . compilation-warning-face)
      (" ERROR " . compilation-error-face)
      ("^[0-9]* [0-9]*\\.[0-9]* \\[[0-9 ]*\\]" . font-lock-comment-face)
      )                                       ;; FONT-LOCK-LIST
    '("\\(Debug\\|Error\\|Info\\|Root\\.All\\|Warn\\)\\.log$") ;; AUTO-MODE-LIST
    (list
     (lambda ()
       (setq global-auto-revert-ignore-buffer t))
     ) ;; FUNCTION-LIST
    )
  (setq-mode-local np6-kiosk-log-mode font-lock-keywords-only t)

  ;; .np6 and .npsharp mode
  (define-generic-mode np6-mode ;; MODE
    '(";")                      ;; COMMENT-LIST
    '("|")                      ;; KEYWORD-LIST
    nil                         ;; FONT-LOCK-LIST
    '("start\\.\\(np6\\|npsharp\\)")   ;; AUTO-MODE-LIST
    nil                         ;; FUNCTION-LIST
    )

  (when (require 'engine-mode nil t)
    (defengine jira
      "https://jira.rdisoftware.com/secure/QuickSearch.jspa?searchString=%s"
      :keybinding "j"))

  (defun np61-set-exec-dir (directory)
    (interactive "DRoot np61 directory: ")
    (setq np61-exec-dir directory)
    (setq np61-exec-start-cmd "start.bat")
    (setq np61-exec-stop-cmd "stop.bat")
    (setq np61-exec-reset-cmd "clean.bat")
    (setq np61-exec-bin-dir "bin")
    (setq np61-exec-compilation-dir "c:/Dev/np61/bin/Debug-Win32-VS13"))

  (defun np61-start ()
    (interactive)
    (unless (boundp 'np61-exec-dir)
      (call-interactively 'np61-set-exec-dir))
    (with-temp-buffer
      (cd-absolute np61-exec-dir)
      (async-shell-command np61-exec-start-cmd "*np61*")))

  (defun np61-stop ()
    (interactive)
    (unless (boundp 'np61-exec-dir)
      (call-interactively 'np61-set-exec-dir))
    (with-temp-buffer
      (cd-absolute np61-exec-dir)
      (async-shell-command np61-exec-stop-cmd "*np61*")))

  (defun np61-reset ()
    (interactive)
    (unless (boundp 'np61-exec-dir)
      (call-interactively 'np61-set-exec-dir))
    (with-temp-buffer
      (cd-absolute np61-exec-dir)
      (async-shell-command np61-exec-reset-cmd "*np61*")))
  
  (defun np61-copy-compilation ()
    (interactive)
    (unless (boundp 'np61-exec-dir)
      (call-interactively 'np61-set-exec-dir))
    (copy-directory-if-newer np61-exec-compilation-dir
                             (concat np61-exec-dir "/" np61-exec-bin-dir)))

  ;; global keymap
  (defvar np61-global-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map "s" 'np61-set-exec-dir)
      (define-key map "a" 'np61-start)
      (define-key map "o" 'np61-stop)
      (define-key map "r" 'np61-reset)
      (define-key map (kbd "<f5>") 'np61-copy-compilation)
      map)
    "Keymap for global np61 commands")

  (defalias 'np61-keymap np61-global-keymap)
  (global-set-key (kbd "<f5>") 'np61-keymap))
