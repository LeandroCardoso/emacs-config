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

  (defvar npos-path nil "Path for NPOS environment")
  (defvar npos-base-path "~/Documents/env/" "Initial path used by `npos-set-path'")
  (defvar npos-start-cmd "start.bat" "Script used to start NPOS")
  (defvar npos-stop-cmd "stop.bat" "Script used to stop NPOS")
  (defvar npos-clean-cmd "clean.bat" "Script used to clean-up the NPOS environment")
  (defvar np61-bin-path "c:/Dev/np61/bin/Debug-Win32-VS13")
  (defvar npsharp-plugins-path "c:/Dev/pele/NpSharpRoot/Plugins")

  ;; npos application
  (defun npos-set-path ()
    (interactive)
    (setq npos-path (read-directory-name "NPOS directory: " npos-base-path
                                         npos-base-path t)))

  (defun npos-start ()
    (interactive)
    (unless npos-path
      (call-interactively 'npos-set-path))
    (with-temp-buffer
      (cd-absolute npos-path)
      (async-shell-command npos-start-cmd "*npos*")))

  (defun npos-stop ()
    (interactive)
    (unless npos-path
      (call-interactively 'npos-set-path))
    (with-temp-buffer
      (cd-absolute npos-path)
      (async-shell-command npos-stop-cmd "*npos*")))

  (defun npos-clean ()
    (interactive)
    (unless 'npos-path
      (call-interactively 'npos-set-path))
    (with-temp-buffer
      (cd-absolute npos-path)
      (async-shell-command npos-clean-cmd "*npos*")))

  (defun np61-copy-bin (&optional force)
    (interactive "P")
    (unless npos-path
      (call-interactively 'npos-set-path))
    (sync-directories np61-bin-path (concat npos-path "/bin") force))

  (defun npsharp-plugin-copy-bin (&optional force)
    (interactive "P")
    (unless npos-path
      (call-interactively 'npos-set-path))
    (let ((plugin-name (when (string-match (concat npsharp-plugins-path "/\\([^/]+\\)") default-directory)
                         (match-string-no-properties 1 default-directory))))
      (sync-directories (concat npsharp-plugins-path "/" plugin-name
                                "/src/NpSharp.Plugin." plugin-name "/bin/Debug")
                        (concat npos-path "/NpSharpBin/Plugins/" plugin-name)
                        force)))

  ;TODO special plugins: Np6PosCore Np6WayCore Sales

  ;; global keymap
  (defvar npos-global-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map "s" 'npos-set-path)
      (define-key map "a" 'npos-start)
      (define-key map "o" 'npos-stop)
      (define-key map "r" 'npos-clean)
      (define-key map (kbd "<f5>") 'np61-copy-bin)
      map)
    "Keymap for global npos commands")

  (defalias 'npos-keymap npos-global-keymap)
  (global-set-key (kbd "<f5>") 'npos-keymap)

  ;; engine
  (when (require 'engine-mode nil t)
    (defengine jira-rdi
      "https://jira.rdisoftware.com/secure/QuickSearch.jspa?searchString=%s"
      :keybinding "j")
    (defengine jira-mcd
      "https://us-jira.mcd.com/secure/QuickSearch.jspa?searchString=%s"
      :keybinding "d"))

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
