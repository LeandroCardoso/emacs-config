(when (eq system-type 'windows-nt)


(setenv "PATH" "c:\\Windows\\System32")

;; root directories are added in the beggining
(add-unix-root-dir "c:\\msys32\\mingw32")
(add-unix-root-dir "c:\\msys32")


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

(defun msdn-search (QUERY)
  "Query MSDN for the QUERY string parameter"
  (interactive (list (read-string (concat "MSDN (" (thing-at-point 'symbol t) "): ")
                                  nil
                                  'msdn-search-history
                                  (thing-at-point 'symbol t)
                                  nil)))
  (browse-url
   (concat "https://duckduckgo.com/?q=!ducky+"
           QUERY
           "+site:msdn.microsoft.com")))

(defun cppreference (QUERY)
  "Query cppreference for the QUERY string parameter"
  (interactive (list (read-string (concat "cppreference (" (thing-at-point 'symbol t) "): ")
                                  nil
                                  'cppreference-search-history
                                  (thing-at-point 'symbol t)
                                  nil)))
  (browse-url
   (concat "https://duckduckgo.com/?q=!cppr+"
           (url-hexify-string QUERY))))

(defun google (QUERY)
  "Query google for the QUERY string parameter"
  (interactive (list (read-string (concat "google (" (thing-at-point 'symbol t) "): ")
                                  nil
                                  'google-search-history
                                  (thing-at-point 'symbol t)
                                  nil)))
  (browse-url
   (concat "https://www.google.com/search?q="
           (url-hexify-string QUERY))))

(defun shell-bash ()
  "Run `shell' with bash"
  (interactive)
  (let ((explicit-shell-file-name (executable-find "bash"))
	(shell-file-name "bash"))
    (setenv "SHELL" shell-file-name)
    (call-interactively 'shell)))

(defun term-bash ()
  "Run `term' with bash"
  (interactive)
  (let ((explicit-shell-file-name (executable-find "bash"))
	(shell-file-name "bash"))
    (setenv "SHELL" shell-file-name)
    (call-interactively 'term)))

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
  ;; undefined without project
  (setq compile-command "build.cmd /p:Platform=x86 /p:Configuration=Release /t:Build")
)
