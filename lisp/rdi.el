(require 'project)
(require 'msvs)
(require 'fragment)
(require 'xml-format)

(defconst np6-bugs-root-directory
  (if (eq system-type 'windows-nt) "~/Documents/bugs/" "~/dev/rdi/bugs/")
  "Development bugs root directory")
(defconst np6-env-root-directory
  (if (eq system-type 'windows-nt) "~/Documents/env/" "~/dev/rdi/env/")
  "Development environment root directory")
(defconst np6-plugins-src-directory
  (if (eq system-type 'windows-nt) "c:/Dev/NpSharpRoot/Plugins/" "~/dev/rdi/src/NpSharpRoot/Plugins/")
  "Source code directory for np# plugins")
(defconst np6-np61-src-directory
  (if (eq system-type 'windows-nt) "c:/Dev/np61/" "~/dev/rdi/src/np61/")
  "Source code directory for np61 core")

;; Project
(add-to-list 'project-root-up-directory-list np6-bugs-root-directory)
(add-to-list 'project-root-up-directory-list np6-env-root-directory)

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

;; np6 log key
(defvar np6-log-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in np6 log buffers.")

(define-key np6-log-mode-map (kbd "C-c C-v") 'np6-view-fragment-display-other-window)
(define-key np6-log-mode-map (kbd "C-c C-p") 'np6-prodinfo-fragment-display-other-window)

(defun np6-log-mode-setup ()
  (use-local-map np6-log-mode-map))

;; np6 log mode
(define-generic-mode np6-log-mode                      ; MODE
  nil                                                  ; COMMENT-LIST
  nil                                                  ; KEYWORD-LIST
  '(("^\\<\\(INFO\\|DEBUG\\)\\>" . font-lock-function-name-face)
    ("^\\<WARNING\\>" . compilation-warning-face)
    ("^\\<\\(ERROR\\|FATAL\\)\\>" . compilation-error-face)
    ("\t.*\t" . font-lock-comment-face)
    ("Legacy [a-zA-Z]+ Log" . font-lock-comment-face)) ; FONT-LOCK-LIST
  '("[0-9]\\{8\\}\\(_DEBUG\\)?-[0-9]\\{3\\}\\.log$")   ; AUTO-MODE-LIST
  '(np6-log-mode-setup))                               ; FUNCTION-LIST

;; Ugly hack to disable automatic string highlight. This is disabled due to several malformed
;; strings.
(require 'mode-local)
(setq-mode-local np6-log-mode font-lock-keywords-only t)

;; np6 production log mode
(define-generic-mode np6-prod-log-mode                    ; MODE
  nil                                                     ; COMMENT-LIST
  nil                                                     ; KEYWORD-LIST
  '(("\\(INFO\\|FINEST\\|DEBUG\\)" . font-lock-function-name-face)
    ("WARNING" . compilation-warning-face)
    ("\\(ERROR\\|FATAL\\)" . compilation-error-face)
    ("^[0-9- :]*;" . font-lock-comment-face)              ; timestamp
    ("\\[com.*\\] Thread: .*$" . font-lock-comment-face)) ; FONT-LOCK-LIST
  '("\\(newposv6\\|np6[a-z]*\\)-[0-9]\\.[0-9]\\.log$")    ; AUTO-MODE-LIST
  '(np6-log-mode-setup))                                  ; FUNCTION-LIST

;; np6 kiosk log mode
(define-generic-mode np6-kiosk-log-mode                                 ; MODE
  nil                                                                   ; COMMENT-LIST
  nil                                                                   ; KEYWORD-LIST
  '((" \\(INFO\\|DEBUG\\) " . font-lock-function-name-face)
    (" WARN " . compilation-warning-face)
    (" ERROR " . compilation-error-face)
    ("^[0-9]* [0-9]*\\.[0-9]* \\[[0-9 ]*\\]" . font-lock-comment-face)) ; FONT-LOCK-LIST
  '("\\(Debug\\|Error\\|Info\\|Root\\.All\\|Warn\\)\\.log$")            ; AUTO-MODE-LIST
  '(np6-log-mode-setup))                                                ; FUNCTION-LIST

(setq-mode-local np6-kiosk-log-mode font-lock-keywords-only t)

(add-to-list 'global-auto-revert-ignore-modes 'np6-log-mode)
(add-to-list 'global-auto-revert-ignore-modes 'np6-prod-log-mode)
(add-to-list 'global-auto-revert-ignore-modes 'np6-kiosk-log-mode)

;; .np6 and .npsharp mode
(defvar np6-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in np6 buffers.")

(define-key np6-mode-map (kbd "<tab>") 'company-indent-or-complete-common)

(defun np6-mode-setup ()
  (use-local-map np6-mode-map))

(define-generic-mode np6-mode        ; MODE
  '(";")                             ; COMMENT-LIST
  '("|")                             ; KEYWORD-LIST
  nil                                ; FONT-LOCK-LIST
  '("start.*\\.\\(np6\\|npsharp\\)") ; AUTO-MODE-LIST
  '(np6-mode-setup))                 ; FUNCTION-LIST


;; flycheck-clang
(when (require 'flycheck nil t)
  (setq flycheck-clang-ms-extensions t)
  (setq flycheck-clang-warnings '("all" "extra" "no-invalid-token-paste"))

  (setq flycheck-clang-definitions nil)
  (dolist (def '("_MSC_VER=1800" ; used by windows
                 "_M_IX86"
                 "_WIN32"
                 "NPMODDEF"      ; used by np61
                 "XP_WIN"        ; used by js180
                 ))
    (push def flycheck-clang-definitions)))

;; git-link
(defun git-link-bitbucket-rdi (hostname dirname filename _branch commit start end)
  (format "https://%s/%s/browse/%s?%s%s"
          hostname
          (string-replace "scm/np" "projects/NP/repos"                             ;np61
                          (string-replace "scm/npl" "projects/NPL/repos" dirname)) ;np#
          filename
          (if _branch
              (concat "at=refs%2Fheads%2F" _branch)
            (concat "at=" commit))
          (if start
            (if end
                (format "#%s-%s" start end)
              (format "#%s" start))
            "")))

(defun git-link-commit-bitbucket-rdi (hostname dirname commit)
  (format "https://%s/%s/commits/%s"
	  hostname
      (string-replace "scm/np" "projects/NP/repos"                             ;np61
                      (string-replace "scm/npl" "projects/NPL/repos" dirname)) ;np#
	  commit))

(with-eval-after-load "git-link"
  (add-to-list 'git-link-remote-alist '("git.rdisoftware.com" git-link-bitbucket-rdi))
  (add-to-list 'git-link-commit-remote-alist '("git.rdisoftware.com" git-link-commit-bitbucket-rdi))
  (add-to-list 'git-link-homepage-remote-alist '("git.rdisoftware.com" git-link-homepage-github)))

;; newpos
(defun np6-plugin-name (&optional directory)
  (let ((dir (or directory default-directory)))
    (when (and np6-plugins-src-directory
               (string-match (concat (expand-file-name np6-plugins-src-directory) "\\([^/]+\\)")
                             (expand-file-name dir)))
      (match-string-no-properties 1 dir))))

(defun np6-np61-project-p (&optional directory)
  (let ((pr (project-current nil (or directory default-directory))))
    (when (and np6-np61-src-directory pr)
      (string= (expand-file-name (file-name-as-directory np6-np61-src-directory))
               (expand-file-name (file-name-as-directory (project-root pr)))))))

;; Environment setup
(when (eq system-type 'windows-nt)
  (defvar np6-env-directory nil "Development environment directory")
  (defvar np6-debug t "Copy Debug binaries, instead of Release binaries")

  (defun np6-np61-dest-directory ()
    (let ((poscore-dir (concat np6-env-directory "NpSharpBin/Plugins/Np6PosCore")))
      (if (file-directory-p poscore-dir)
          poscore-dir
        (concat np6-env-directory "bin"))))

  (defun np6-config ()
    (interactive)
    (when (or (called-interactively-p t)
              (not np6-env-directory))
      (setq np6-env-directory
            (read-directory-name "NP6 environment directory: " np6-env-root-directory nil t))
      (setq np6-debug (yes-or-no-p "Copy Debug binaries? "))))

  (defun np6-config-info()
    (interactive)
    (if np6-env-directory
        (let ((plugin-name (np6-plugin-name)))
          (message "NP6 directory:[%s], debug:[%s], np61 destination:[%s], current project:[%s] core:[%s]"
                   np6-env-directory np6-debug (file-name-base (np6-np61-dest-directory))
                   (cond (plugin-name plugin-name)
                         ((np6-np61-project-p) "np61")
                         (t nil))
                   (when plugin-name
                     (file-directory-p (concat np6-plugins-src-directory plugin-name "/core/")))))
      (message "Np6 config no set")))

  (defun np6-execute-script ()
    (interactive)
    (np6-config)
    (let* ((cmd (completing-read (concat "Script [" np6-env-directory "]: ")
                                 (directory-files np6-env-directory nil "\\.bat") nil t nil))
           (full-cmd (concat np6-env-directory cmd))
           (default-directory (file-name-directory full-cmd)))
      (when (and cmd (not (string-empty-p cmd)))
        (start-process cmd "*np6*" full-cmd)
        (view-buffer "*np6*"))))

  (defun np6-copy-bin (&optional ignore-timestamp)
    (interactive "P")
    (np6-config)
    (let ((plugin-name (np6-plugin-name)))
      (cond (plugin-name
             ;; copy plugin
             (let ((src-dir (concat np6-plugins-src-directory plugin-name
                                    "/src/NpSharp.Plugin." plugin-name
                                     (if np6-debug "/bin/Debug" "/bin/Release")))
                   (dest-dir (concat np6-env-directory "NpSharpBin/Plugins/" plugin-name)))
               (sync-directories src-dir dest-dir ignore-timestamp))
             ;; copy core submodule
             (let* ((core-dir (concat np6-plugins-src-directory plugin-name "/core/"))
                    (src-dir (concat core-dir (if np6-debug "bin/Debug-Win32-VS13" "bin/Release-Win32-VS13")))
                    (dest-dir (concat np6-env-directory "NpSharpBin/Plugins/" plugin-name
                                       ;; Sale uses a different destination directory
                                      (when (eq (compare-strings plugin-name nil nil
                                                                 "sale" nil nil
                                                                 t) t)
                                         "/accountingServiceBin"))))
               (when (file-directory-p core-dir)
                 (sync-directories src-dir dest-dir ignore-timestamp))))
            ;; copy np61
            ((np6-np61-project-p)
             (let ((src-dir (concat np6-np61-src-directory
                                     (if np6-debug "bin/Debug-Win32-VS13" "bin/Release-Win32-VS13")))
                   (dest-dir (np6-np61-dest-directory)))
               (sync-directories src-dir dest-dir ignore-timestamp)))
            (t (error "No NP6 project detected")))))

  ;; keymap
  (defvar np6-keymap nil "Keymap for global NP6 commands")
  (setq np6-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "<f5>") 'np6-config)
          (define-key map "i" 'np6-config-info)
          (define-key map "x" 'np6-execute-script)
          (define-key map "c" 'np6-copy-bin)
          map))
  (defalias 'np6-keymap np6-keymap)
  (global-set-key (kbd "<f5>") 'np6-keymap))


;; np61 include path list
(defvar np61-include-path-list nil
  "A list of include paths for np61 project.")

(defun np61-update-include-path-list ()
  "Update the `np61-include-path-list'."
  (interactive)
  (if np6-np61-src-directory
      (progn
        (setq np61-include-path-list nil)
        (message "Updating np61-update-include-path-list...")
        (let ((start-time (current-time))
              (pr np6-np61-src-directory))
          (dolist (dir (nconc (directory-list (concat pr "src/"))
                               (directory-list (concat pr "extSrc/"))))
            ;; Skip directories that do not have header files
            (when (directory-files dir nil "\\.h.*" t)
              (push dir np61-include-path-list)))
          (message "Updating np61-update-include-path-list...done in %g seconds"
                   (float-time (time-since start-time)))))
    (error "Np6 core not found")))

(defun np61-c-c++-setup ()
  "Set `flycheck-clang-include-path',
`company-clang-arguments' nad `company-c-headers-path-user' with
np61 and compiler directories."
  (interactive)
  (when (np6-np61-project-p)
    ;; update np61-include-path-list
    (when (null np61-include-path-list) (np61-update-include-path-list))
    ;; update flycheck-clang-include-path
    (setq-local flycheck-clang-include-path np61-include-path-list)
    (when msvs-include-directory (push msvs-include-directory flycheck-clang-include-path))
    (when msvs-platform-sdk (push msvs-platform-sdk flycheck-clang-include-path))
    ;; update company-clang-arguments
    (setq-local company-clang-arguments
                (mapcar (lambda (dir) (concat "-I" dir)) np61-include-path-list))
    (when msvs-include-directory (push (concat "-I" msvs-include-directory)
                                       company-clang-arguments))
    (when msvs-platform-sdk (push (concat "-I" msvs-platform-sdk) company-clang-arguments))
    ;; update company-c-headers
    (setq-local company-c-headers-path-user np61-include-path-list))
  nil)

(add-hook 'c-mode-hook 'np61-c-c++-setup)
(add-hook 'c++-mode-hook 'np61-c-c++-setup)

(defun rdi-msvs-generate-compile-command ()
  (cond
   ;; c or c++
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode)
        ;; c or c++ project
        (string-match-p msvs-cpp-project-regexp (or buffer-file-name "")))
    (msvs-generate-compile-command t "win32" "Debug" "Build" "/p:PostBuildEventUseInBuild=false"))
   ;; c#
   ((or (eq major-mode 'csharp-mode)
        (string-match-p msvs-cs-project-regexp (or buffer-file-name "")))
    ;; c# project
    (msvs-generate-compile-command nil "\"Any CPU\"" "Debug" "Build"))
   ;; solution
   ((string-match-p msvs-solution-regexp (or buffer-file-name ""))
    (if (np6-np61-project-p)
        (msvs-generate-compile-command nil "\"Mixed Platforms\"" "Debug" "Build" "/p:PostBuildEventUseInBuild=false")
      (msvs-generate-compile-command nil "\"Any CPU\"" "Debug" "Build")))))

(setq msvs-compile-command-function 'rdi-msvs-generate-compile-command)


;; RDI Nuget
(defconst nuget-rdi-source-name "RDI")
(defconst nuget-rdi-username "lcardoso")

(defun nuget-rdi-update-password ()
  "Update the RDI nuget password"
  (interactive)
  (nuget-execute "sources" "update"
                 "-Name" nuget-rdi-source-name
                 "-User" nuget-rdi-username
                 "-pass" (read-passwd "Enter the new password for RDI Nuget source: ")))


;; np6 view
(defun np6-view-auto-format ()
  "Auto format a view file. This function is intended to be used as a hook.

See `xml-format'"
  (save-mark-and-excursion
    (when (and (not (bound-and-true-p np6-view-ignore-auto-format))
               (string-match-p ".*view.*\\.xml$" (or buffer-file-name ""))
               (progn ;only format if file has more than one line
                 (goto-char (point-min))
                 (eq 1 (forward-line 2))))
      (message "Formatting np6 view file: %s. %s to view without formatting."
               (buffer-file-name) (substitute-command-keys "\\[np6-view-revert]"))
      (deactivate-mark)
      (with-silent-modifications
        (xml-format)))))

(defun np6-view-revert ()
  "Revert the current np6 view buffer without auto-formatting it.

See `np6-view-auto-format'"
  (interactive)
  (when (string-match-p ".*view.*\\.xml$" (or buffer-file-name ""))
    (setq-local np6-view-ignore-auto-format (not (bound-and-true-p np6-view-ignore-auto-format)))
    (revert-buffer-with-fine-grain t t)))

(defun np6-view-fragment-display-other-window ()
  "Search for a visible np6 view in the current buffer and display
it in a temporary buffer in another window.

See `fragment-xml-display-other-window'."
  (interactive)
  (let ((case-fold-search t))
    (unless (fragment-xml-display-other-window "view")
      (error "No view found in current buffer"))))

(defun np6-prodinfo-fragment-display-other-window ()
  "Search for a visible np6 prodinfo in the current buffer and
display it in a temporary buffer in another window.

See `fragment-xml-display-other-window'."
  (interactive)
  (let ((case-fold-search t))
    (unless (fragment-xml-display-other-window "prodinfo")
      (error "No prodinfo found in current buffer"))))

(define-key nxml-mode-map (kbd "C-c C-r") 'np6-view-revert)

(add-hook 'nxml-mode-hook 'np6-view-auto-format)
(add-hook 'after-revert-hook 'np6-view-auto-format)
