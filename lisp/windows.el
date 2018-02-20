(when (eq system-type 'windows-nt)

;; root directories are added in the beginning
(add-unix-root-dir "c:\\msys64\\mingw64")
(add-unix-root-dir "c:\\msys64")

;; Add windows_bin to PATH and exec-path
(setenv "PATH" (concat (convert-standard-filename
                        (expand-file-name
                         (concat user-emacs-directory "windows_bin/")))
                       path-separator
                       (getenv "PATH")))
(add-to-list 'exec-path (concat user-emacs-directory "windows_bin/"))

;; nodejs
(when (file-exists-p "c:/Program Files/nodejs/nodevars.bat")
  (setq explicit-cmdproxy.exe-args '("/k \"\"C:\\Program Files\\nodejs\\nodevars.bat\"\"")))

;; flycheck
;; launch external process is slow in Windows, so we don't want to use the new-line option
(setq flycheck-check-syntax-automatically '(save idle-change mode-enable))

;; this is a very recommended setup
(setq w32-pipe-read-delay 0)

;; gtags
(setq ggtags-highlight-tag nil) ;; this is slow in windows
(setenv "GTAGSFORCECPP" "1")
(setenv "GTAGSLIBPATH" (concat "C:\\Program Files\\Microsoft SDKs\\Windows\\v7.1A\\Include:"
                               "C:\\Program Files\\Microsoft Visual Studio 12.0\\VC\\include"))

;; Hack to maximize frame in Windows
(add-hook 'after-make-frame-functions
          (lambda(FRAME)
            (modify-frame-parameters FRAME '((fullscreen . maximized)))))


;; (add-to-list 'tags-table-list "c:/DBDProj/TAGS.MS-SDK")
;; (add-to-list 'tags-table-list "c:/DBDProj/TAGS.VC")

;; Hooks
;; (defun my-xml-hook ()
  ;; (setq indent-tabs-mode (not (string-match-p "\\.vcxproj" (buffer-name))))
  ;; (setq indent-tabs-mode (not (string-match-p "\\.msbuild" (buffer-name))))
  ;; )

;; (add-hook 'nxml-mode-hook 'my-xml-hook)


;; Functions

(defun shell-bash ()
  "Run `shell' with bash"
  (interactive)
  (let ((explicit-shell-file-name (executable-find "bash"))
        (shell-file-name "bash"))
    (setenv "SHELL" explicit-shell-file-name)
    (setenv "EMACS" "t")
    (call-interactively 'shell)))
)
