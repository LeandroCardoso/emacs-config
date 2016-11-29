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
   "C:\\Users\\santosl4\\Documents\\bin;"
   "C:\\Users\\santosl4\\Documents\\script;"
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

;; Variables
(setq mydrive "e:/")


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
              (cond
               ;; tfs odyssey or opteva
               ((or (string-match-p "Odyssey" project-root-dir)
                    (string-match-p "Opteva" project-root-dir))
                (setq compile-command
                      (concat "build.cmd"
                              " /p:SolutionDir=" (convert-standard-filename project-root-dir) "Src\\"
                              " /p:Platform=x86"
                              " /p:Configuration=Release /t:Build"))
                (setq compilation-directory-output
                      (concat project-root-dir "Src/bin/Release/")))
               ;; clearcase opteva
               ((file-exists-p (concat project-root-dir "view.dat"))
                (setq compile-command "build.cmd /p:Platform=x86 /p:Configuration=Release /t:Build")
                (setq compilation-directory-output
                      (concat project-root-dir "XFSOPT_SRC/Src/Src/bin/Release/")))
               ;; undefined with project
               (t
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
          (setq compilation-directory-output "Release/"))))

(defun odyssey-set-project ()
  "TODO"
  (interactive)
  (setq semantic-dependency-include-path
        (mapcar (lambda (STR) (concat (project-root) STR))
                '("Src/Sdk/AMI/Inc" "Src/Sdk/XFS/Inc" "Src/Common/*" "Src/Common/ClassHandlers"))))



(defun msvs-copy-bin-to-drive ()
  "Copy compilated binary files in `compilation-directory-output' set by `msvs-set-compile-command'
to the pen-drive defined by `mydrive'.
Use project name directory as destination directory when it exists."
  (interactive)
  (when (and (stringp mydrive)
             (file-directory-p mydrive)
             (stringp compilation-directory-output)
             (file-directory-p compilation-directory-output))
    (let* ((project-root-dir (project-root))
          (dest-dir
           (concat mydrive
                   (if (stringp project-root-dir)
                       (file-name-nondirectory (directory-file-name project-root-dir)) "bin")
                   "/"))
          (files-copied 0))
      (unless (file-directory-p dest-dir)
        (make-directory dest-dir))
      (dolist (FILE (directory-files compilation-directory-output t "\\(dll\\|exe\\|msi\\|pdb\\)$"))
        (when (file-newer-than-file-p FILE (concat dest-dir (file-name-nondirectory FILE)))
          (message "Copying %s to %s" FILE dest-dir)
          (copy-file FILE dest-dir t)
          (setq files-copied (1+ files-copied))))
      (message "%d files Copied from %s to %s." files-copied compilation-directory-output dest-dir))))

(defun xfs-start (&optional ARG)
  "Start the Diebold XFS windows service.
  nil or 0 : start
  1 : stop
  2 : restart"
  (interactive "P")
  (message
   (cond
    ((or (null ARG) (= ARG 0)) "Starting XFS")
    ((= ARG 1) "Stoping XFS")
    ((= ARG 2) "Restarting XFS")))
  (w32-service-start "Diebold XFS" ARG))


(defun xfs-stop()
  "Stop the Diebold XFS windows service."
  (interactive)
  (xfs-start 1))


(defun xfs-restart()
  "Restart the Diebold XFS windows service."
  (interactive)
  (xfs-start 2))


(defun ami-start (&optional ARG)
  "Start the Diebold XFS and Devices Service windows service.
  nil or 0 : start
  1 : stop
  2 : restart"
  (interactive "P")
  (message
   (cond
    ((or (null ARG) (= ARG 0)) "Starting AMI")
    ((= ARG 1) "Stoping AMI")
    ((= ARG 2) "Restarting AMI")))
  (w32-service-start "Diebold Devices Service" ARG))


(defun ami-stop()
  "Stop the Diebold XFS and Devices Service windows service."
  (interactive)
  (ami-start 1))


(defun ami-restart()
  "Restart the Diebold XFS and Devices Service windows service."
  (interactive)
  (ami-start 2))

;; TODO save in .dir-locals.el
;; add-dir-local-variable
(defun project-set-directory (ROOT_DIR BIN_DIR)
  "Set project directories"
  (require 'find-file)
  (add-to-list 'cc-search-directories (concat ROOT_DIR "/*/*/*/*/*/*") t)
  (add-to-list 'tags-table-list (concat ROOT_DIR "/TAGS")))


(defun directory-backup (DIR)
  "Copy .dll and .exe files from DIR to DIR/backup
Don't overwrite files from previous backup"
  (setq BACKUP_DIR (concat DIR "\\backup"))
  (when (file-exists-p DIR)
    (unless (file-exists-p BACKUP_DIR)
      (make-directory BACKUP_DIR))
    (dolist (SRC_FILE (directory-files DIR t "dll$")) (ignore-errors (copy-file SRC_FILE BACKUP_DIR nil t)))
    (dolist (SRC_FILE (directory-files DIR t "exe$")) (ignore-errors (copy-file SRC_FILE BACKUP_DIR nil t)))))


(defun ami-backup ()
  "Backup AMI exe/dll files"
  (interactive)
  (directory-backup "C:\\Program Files\\Diebold\\AMI"))


(defun xfs-backup ()
  "Backup AMI exe/dll files"
  (interactive)
  (directory-backup "C:\\Program Files\\Diebold\\AgilisXFS\\bin"))


(defun directory-restore (DIR)
  "Copy .dll and .exe files from DIR/backup to DIR.
Delete .pbd files from DIR"
  (setq BACKUP_DIR (concat DIR "\\backup"))
  (when (file-exists-p BACKUP_DIR)
    (dolist (SRC_FILE (directory-files BACKUP_DIR t "dll$")) (with-demoted-errors (copy-file SRC_FILE DIR t t)))
    (dolist (SRC_FILE (directory-files BACKUP_DIR t "exe$")) (with-demoted-errors (copy-file SRC_FILE DIR t t)))
    (dolist (TARGET_FILE (directory-files DIR t "pdb$")) (with-demoted-errors (delete-file TARGET_FILE)))))


(defun ami-restore()
  "Restore AMI exe/dll files from previous backup"
  (interactive)
  (directory-restore "C:\\Program Files\\Diebold\\AMI"))


(defun xfs-restore()
  "Restore XFS exe/dll files from previous backup"
  (interactive)
  (directory-restore "C:\\Program Files\\Diebold\\AgilisXFS\\bin"))


(defun directory-install (DIR TARGET_DIR)
  "Copy files .dll and .exe files from DIR to TARGET_DIR if they exist in the TARGET_DIR
Copy .pdb files from DIR to TARGET_DIR if a .dll or .exe with the same base name was copied"
  (when (file-exists-p TARGET_DIR)
    (dolist (FILE (directory-files DIR nil "\\(dll\\|exe\\)$"))
      (when (file-exists-p (concat TARGET_DIR "\\" FILE))
        (message "Coping file %s" (concat DIR "\\" FILE))
        (with-demoted-errors (copy-file (concat DIR "\\" FILE) TARGET_DIR t t))
        (setq PDB_FILE (replace-regexp-in-string "\\(dll\\|exe\\)$" "pdb" FILE t))
        (when (file-exists-p (concat DIR "\\" PDB_FILE))
          (with-demoted-errors (copy-file (concat DIR "\\" PDB_FILE) TARGET_DIR t t))
          (message "Coping file %s" (concat DIR "\\" PDB_FILE)))))
    (message "Done")))


(defun ami-install ()
  "Copy files from Release binary directory to AMI installation directory"
  (interactive)
  (if (not (boundp 'proj_bin))
      (error "Project not set. Use project-set-dir")
    (if (w32-service-running "Diebold Devices Service")
        (progn
          (ami-stop)
          (directory-install proj_bin "C:\\Program Files\\Diebold\\AMI")
          (ami-start))
      (directory-install proj_bin "C:\\Program Files\\Diebold\\AMI"))))


(defun xfs-install ()
  "Copy files from Release binary directoy to XFS installation directory"
  (interactive)
  (if (not (boundp 'proj_bin))
      (error "Project not set. Use project-set-dir")
    (if (w32-service-running "Diebold XFS")
        (progn
          (xfs-stop)
          (directory-install proj_bin "C:\\Program Files\\Diebold\\AgilisXFS\\bin")
          (xfs-start))
      (directory-install proj_bin "C:\\Program Files\\Diebold\\AgilisXFS\\bin"))))

(defun insert-tag-comment (&optional ARG)
  "Insert a comment in the form (TAG) using the TAG set by `set-tag-comment'.
`comment-indent' is used to the indent.
if ARG is provided then call interactively `set-tag-comment'."
  (interactive "*P")
  (when (or ARG (null (boundp 'tag-comment)))
    (call-interactively 'set-tag-comment))
  (comment-indent)
  (insert "(" tag-comment ")"))


(defun set-tag-comment (TAG)
  "Set the TAG comment to be used by `insert-tag-comment'.
TAG comment becomes buffer local."
  (interactive "sEnter TAG comment: ")
  (setq-local tag-comment TAG))


(defun delete-readme (DIRECTORY)
  "delete all Readme_XXX.html files in DIRECTORY and its subdirectories"
  (interactive "DRoot directory: ")
  (with-output-to-temp-buffer "*delete files*"
    (dolist (FILE (find-lisp-find-files DIRECTORY "^Readme_[A-Z0-9]*\\.html$"))
      (prin1 FILE)
      (terpri)
      (delete-file FILE t)
      )))


;; keybindings
(global-set-key (kbd "<f5>") 'msvs-copy-bin-to-drive)
(global-set-key (kbd "M-<f5>") 'msvs-set-compile-command)
)
