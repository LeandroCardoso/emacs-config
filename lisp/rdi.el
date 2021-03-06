(require 'project)

(defconst np6-bugs-path (if (eq system-type 'windows-nt) "~/Documents/bugs/" "~/dev/rdi/bugs/"))
(defconst np6-env-path (if (eq system-type 'windows-nt) "~/Documents/env/" "~/dev/rdi/env/"))
(defconst np6-plugins-src-path
  (if (eq system-type 'windows-nt) "c:/Dev/NpSharpRoot/Plugins/" "~/dev/rdi/src/NpSharpRoot/Plugins/")
  "Source code path for np# plugins")
(defconst np6-np61-src-path (if (eq system-type 'windows-nt) "c:/Dev/np61/" "~/dev/rdi/src/np61/")
  "Source code path for np61 core")

;; Project
(add-to-list 'project-root-list np6-bugs-path)
(add-to-list 'project-root-list np6-env-path)

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
(add-to-list 'auto-mode-alist '("STLD\\.raw" . nxml-mode))

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

;; engine
(when (require 'engine-mode nil t)
  (defengine jira-rdi
    "https://jira.rdisoftware.com/jira/secure/QuickSearch.jspa?searchString=%s"
    :keybinding "j")
  (defengine jira-mcd
    "https://us-jira.mcd.com/secure/QuickSearch.jspa?searchString=%s"
    :keybinding "J"))

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
    (push def flycheck-clang-definitions)))

(defun np6-plugin-name (&optional path)
  (let ((path (or path default-directory)))
    (when (and np6-plugins-src-path
               (string-match (concat (expand-file-name np6-plugins-src-path) "\\([^/]+\\)")
                             (expand-file-name path)))
      (match-string-no-properties 1 path))))

;; Environment setup
(when (eq system-type 'windows-nt)

  (defvar np6-path nil "Path for NP6 environment")
  (defvar np6-core-dest nil
    "Map of destination names to destination paths to copy np61 core binaries")
  (defvar np6-debug t "Copy Debug binaries, instead of Release binaries")

  (defun np6-config ()
    (interactive)
    (when (or (called-interactively-p)
              (not np6-path))
      (setq np6-path (read-directory-name "NP6 environment directory: " np6-env-path nil t))
      (setq np6-core-dest nil)
      (dolist (dest '(("np61" . "bin") ;TODO unify np61 and poscore
                      ("posCore" . "NpSharpBin/Plugins/Np6PosCore")
                      ("wayCore" . "NpSharpBin/Plugins/Np6WayCore")
                      ("sale" . "NpSharpBin/Plugins/Sale/accountingServiceBin")))
        (when (yes-or-no-p (format "Copy to %s? " (car dest)))
          (push dest np6-core-dest)))
      (setq np6-debug (yes-or-no-p "Copy Debug binaries? "))))

  (defun np6-config-info()
    (interactive)
    (message "NP6 path:[%s] core:%s debug:%s" np6-path (mapcar 'car np6-core-dest) np6-debug))

  (defun np6-execute-script ()
    (interactive)
    (np6-config)
    (let* ((cmd (completing-read (concat "Script [" np6-path "]: ")
                                 (directory-files np6-path nil "\\.bat") nil t nil))
           (full-cmd (concat np6-path cmd))
           (default-directory (file-name-directory full-cmd)))
      (when (and cmd (not (string-empty-p cmd)))
        (start-process cmd "*np6*" full-cmd)
        (view-buffer "*np6*"))))

  (defun np6-plugin-copy-bin (&optional force)
    (interactive "P")
    (let ((plugin-name (np6-plugin-name)))
      (if plugin-name
          (progn
            (np6-config)
            (sync-directories (concat np6-plugins-src-path plugin-name "/src/NpSharp.Plugin." plugin-name
                                      (if np6-debug "/bin/Debug" "/bin/Release"))
                              (concat np6-path "NpSharpBin/Plugins/" plugin-name)
                              force))
        (error "Plugin not found"))))

  (defun np6-core-copy-bin (&optional force)
    (interactive "P")
    (if np6-np61-src-path
        (progn
          (np6-config)
          (dolist (dest-dir np6-core-dest)
            (let ((dest-path (concat np6-path (cdr dest-dir))))
              (when (file-directory-p dest-path)
                (sync-directories (concat np6-np61-src-path
                                          (if np6-debug "bin/Debug-Win32-VS13" "bin/Release-Win32-VS13"))
                                  dest-path force)))))
      (error "Np6 core not found")))

  (defun np6-copy-bin-dwim (&optional force)
    (interactive "P")
    (if (np6-plugin-name)
        (np6-plugin-copy-bin force)
      (np6-core-copy-bin force)))

  ;; keymap
  (defvar np6-keymap nil "Keymap for global NP6 commands")
  (setq np6-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "<f5>") 'np6-config)
          (define-key map "i" 'np6-config-info)
          (define-key map "x" 'np6-execute-script)
          (define-key map "c" 'np6-copy-bin-dwim)
          (define-key map "P" 'np6-plugin-copy-bin)
          (define-key map "C" 'np6-core-copy-bin)
          map))
  (defalias 'np6-keymap np6-keymap)
  (global-set-key (kbd "<f5>") 'np6-keymap))


;; np61 include path list
(defvar np61-include-path-list nil
  "A list of include paths for np61 project.")

(defun np61-update-include-path-list ()
  "Update the `np61-include-path-list'."
  (interactive)
  (if np6-np61-src-path
      (progn
        (setq np61-include-path-list nil)
        (message "Updating np61-update-include-path-list...")
        (let ((start-time (current-time))
              (pr np6-np61-src-path))
          (dolist (path (nconc (directory-list (concat pr "src/"))
                               (directory-list (concat pr "extSrc/"))))
            ;; Skip directories that do not have header files
            (when (directory-files path nil "\\.h.*" t)
              (push path np61-include-path-list)))
          (message "Updating np61-update-include-path-list...done in %g seconds"
                   (float-time (time-since start-time)))))
    (error "Np6 core not found")))

(defun np61-c-c++-setup ()
  "Set `flycheck-clang-include-path',
`company-clang-arguments' nad `company-c-headers-path-user' with
np61 and compiler directories."
  (interactive)
  (when (string= (expand-file-name np6-np61-src-path)
                 (expand-file-name (car (project-roots (project-current)))))
    ;; update np61-include-path-list
    (when (null np61-include-path-list) (np61-update-include-path-list))
    ;; update flycheck-clang-include-path
    (setq-local flycheck-clang-include-path np61-include-path-list)
    (when msvs-include-directory (push msvs-include-directory flycheck-clang-include-path))
    (when msvs-platform-sdk (push msvs-platform-sdk flycheck-clang-include-path))
    ;; update company-clang-arguments
    (setq-local company-clang-arguments
                (mapcar (lambda (path) (concat "-I" path)) np61-include-path-list))
    (when msvs-include-directory (push (concat "-I" msvs-include-directory)
                                       company-clang-arguments))
    (when msvs-platform-sdk (push (concat "-I" msvs-platform-sdk) company-clang-arguments))
    ;; update company-c-headers
    (setq-local company-c-headers-path-user np61-include-path-list))
  nil)

(add-hook 'c-mode-hook 'np61-c-c++-setup)
(add-hook 'c++-mode-hook 'np61-c-c++-setup)
