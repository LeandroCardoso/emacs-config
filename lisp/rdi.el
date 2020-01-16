(when (string= (getenv "USERDOMAIN") "RDISOFTWARE")
  (require 'project)
  (add-to-list 'project-root-list "~/Documents/bugs/")
  (add-to-list 'project-root-list "~/Documents/env/")

  ;; nps
  (when (featurep 'js2-mode)
    (defun nps-setup-hook ()
      (when (string-match-p "\\.nps\\'" (or buffer-file-name ""))
        (setq-local js2-highlight-external-variables nil)
        (setq-local js2-include-browser-externs nil)
        (setq-local js2-language-version 180)))

    (add-hook 'js2-mode-hook #'nps-setup-hook)
    (add-hook 'js2-minor-mode-hook #'nps-setup-hook))

  (add-to-list 'auto-mode-alist '("\\.nps\\'" . js-mode))

  ;; There are some c++ files using .c extension.
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-or-c++-mode))

  (add-to-list 'auto-mode-alist '("product\\.specification" . nxml-mode))

  ;; Force coding system in log files
  (modify-coding-system-alist 'file "\\.log\\'" 'prefer-utf-8-dos)

  ;; np6 log mode
  (define-generic-mode np6-log-mode ;; MODE
    nil                             ;; COMMENT-LIST
    nil                             ;; KEYWORD-LIST
    '(("^\\<\\(INFO\\|DEBUG\\)\\>" . font-lock-function-name-face)
      ("^\\<WARNING\\>" . compilation-warning-face)
      ("^\\<\\(ERROR\\|FATAL\\)\\>" . compilation-error-face)
      ("\t.*\t" . font-lock-comment-face)
      ("Legacy [a-zA-Z]+ Log" . font-lock-comment-face)
      )                                                ;; FONT-LOCK-LIST
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
      )                                                  ;; FONT-LOCK-LIST
    '("\\(newposv6\\|np6[a-z]*\\)-[0-9]\\.[0-9]\\.log$") ;; AUTO-MODE-LIST
    (list
     (lambda ()
       (setq global-auto-revert-ignore-buffer t))
     ) ;; FUNCTION-LIST
    )

  ;; np6 kiosk log mode
  (define-generic-mode np6-kiosk-log-mode ;; MODE
    nil                                   ;; COMMENT-LIST
    nil                                   ;; KEYWORD-LIST
    '((" \\(INFO\\|DEBUG\\) " . font-lock-function-name-face)
      (" WARN " . compilation-warning-face)
      (" ERROR " . compilation-error-face)
      ("^[0-9]* [0-9]*\\.[0-9]* \\[[0-9 ]*\\]" . font-lock-comment-face)
      )                                                        ;; FONT-LOCK-LIST
    '("\\(Debug\\|Error\\|Info\\|Root\\.All\\|Warn\\)\\.log$") ;; AUTO-MODE-LIST
    (list
     (lambda ()
       (setq global-auto-revert-ignore-buffer t))
     ) ;; FUNCTION-LIST
    )
  (setq-mode-local np6-kiosk-log-mode font-lock-keywords-only t)

  ;; .np6 and .npsharp mode
  (define-generic-mode np6-mode        ;; MODE
    '(";")                             ;; COMMENT-LIST
    '("|")                             ;; KEYWORD-LIST
    nil                                ;; FONT-LOCK-LIST
    '("start.*\\.\\(np6\\|npsharp\\)") ;; AUTO-MODE-LIST
    nil                                ;; FUNCTION-LIST
    )

  ;; np61 application
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

  (defun np61-copy-compilation (&optional force)
    (interactive "P")
    (unless (boundp 'np61-exec-dir)
      (call-interactively 'np61-set-exec-dir))
    (copy-directory-common-files np61-exec-compilation-dir
                                 (concat np61-exec-dir "/" np61-exec-bin-dir)
                                 force))

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
  (global-set-key (kbd "<f5>") 'np61-keymap)

  ;; engine
  (when (require 'engine-mode nil t)
    (defengine jira
      "https://jira.rdisoftware.com/secure/QuickSearch.jspa?searchString=%s"
      :keybinding "j"))

  ;; flycheck-clang
  (when (require 'flycheck nil t)
    (setq flycheck-clang-ms-extensions t)
    (setq flycheck-clang-warnings '("all" "extra" "no-invalid-token-paste"))

    (setq flycheck-clang-definitions nil)
    (dolist (def '("_MSC_VER=1800" ;used by windows
                   "_M_IX86"
                   "_WIN32"
                   "NPMODDEF" ;used by np61
                   "XP_WIN"   ;used by js180
                   ))
      (push def flycheck-clang-definitions))

    (defvar np61-include-path-list nil
      "A list of include paths for np61 project.")

    (defun np61-update-include-path-list ()
      "Update the `np61-include-path-list'."
      (interactive)
      (setq np61-include-path-list nil)
      (message "Updating np61-update-include-path-list...")
      (let ((start-time (current-time))
            (pr "c:/Dev/np61/"))
        (dolist (path (nconc (directory-list (concat pr "src/"))
                             (directory-list (concat pr "extSrc/"))))
          ;; Skip directories that do not have header files
          (when (directory-files path nil "\\.h.*" t)
            (push path np61-include-path-list)))
        (message "Updating np61-update-include-path-list...done in %g seconds"
                 (float-time (time-since start-time)))))

    (defun np61-c-c++-setup ()
      "Set `flycheck-clang-include-path' and
`company-clang-arguments' with np61 and compiler directories."
      (interactive)
      (when (string= "c:/Dev/np61/" (car (project-roots (project-current))))
        ;; update np61-include-path-list
        (when (null np61-include-path-list) (np61-update-include-path-list))
        ;; update flycheck-clang-include-path
        (setq-local flycheck-clang-include-path np61-include-path-list)
        (when msvs-include-directory (push msvs-include-directory flycheck-clang-include-path))
        (when msvs-platform-sdk (push msvs-platform-sdk flycheck-clang-include-path))
        ;; update company-clang-arguments
        (setq-local company-clang-arguments
                    (mapcar* (lambda (path) (concat "-I" path)) np61-include-path-list))
        (when msvs-include-directory (push (concat "-I" msvs-include-directory) company-clang-arguments))
        (when msvs-platform-sdk (push (concat "-I" msvs-platform-sdk) company-clang-arguments))
        ;; update company-c-headers
        (setq-local company-c-headers-path-user np61-include-path-list))
      nil)

    (add-hook 'c-mode-hook 'np61-c-c++-setup)
    (add-hook 'c++-mode-hook 'np61-c-c++-setup)))
