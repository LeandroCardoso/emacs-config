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

(setq compile-command "msbuild.cmd /p:Platform=win32 /p:Configuration=Debug /t:Build")


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

(require 'semantic)
(semantic-add-system-include "C:/Program Files/Microsoft SDKs/Windows/v7.1A/Include" 'c-mode)
(semantic-add-system-include "C:/Program Files/Microsoft SDKs/Windows/v7.1A/Include" 'c++-mode)
(semantic-add-system-include "C:/Program Files/Microsoft Visual Studio 12.0/VC/include" 'c-mode)
(semantic-add-system-include "C:/Program Files/Microsoft Visual Studio 12.0/VC/include" 'c++-mode)

;; An ungly hack to idenfity c++ extensionless files as c++ file. Thanks ISO c++, a file without
;; extension was a great idea!
(add-to-list 'auto-mode-alist '("[Ii]nclude" . c++-mode) t)

(setenv "PATH"
  (concat
   "C:\\Program Files\\Microsoft Visual Studio 12.0\\Common7\\Tools;"
   "C:\\Program Files\\Microsoft Visual Studio 12.0\\Common7\\IDE;"
   (getenv "PATH")))

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


;; (make-variable-buffer-local 'compilation-directory-output)


(defun msvs-set-compile-command ()
  "TODO"
  (interactive)
  (let ((project-root-dir (project-root)))
        (if (stringp project-root-dir)
            (progn
              (message "Project Root:%s" project-root-dir)
              (setq compile-command "build.cmd /p:Platform=x86 /p:Configuration=Release /t:Build")
              (setq compilation-directory-output
                    (concat project-root-dir "Release/"))))
        (require 'files-x)
        (create-dir-local-file project-root-dir)
        (modify-dir-local-variable nil 'compile-command compile-command 'add-or-replace)
        (modify-dir-local-variable nil
                                   'compilation-directory-output
                                   compilation-directory-output
                                   'add-or-replace)
        (save-buffer))
)
)
