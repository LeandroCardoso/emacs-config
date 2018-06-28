(when (eq system-type 'windows-nt)

  (defun w32-convert-filename (file-name)
    "Converted slash characters in file names into backslashes."
    (let ((file-name (convert-standard-filename file-name))
          (start 0))
      (while (string-match "/" file-name start)
        (aset file-name (match-beginning 0) ?\\)
        (setq start (match-end 0)))
      file-name))

  (defun add-unix-root-dir (DIRNAME)
    "Set emacs to use an additional custom unix root directory.
Custom directories are added in the begging"
    (when (file-directory-p DIRNAME)
      (require 'woman)
      (require 'info)
      (dolist (DIR '("/usr/bin" "/bin"))
        (when (file-directory-p (concat DIRNAME DIR))
          (setenv "PATH" (concat (w32-convert-filename (concat DIRNAME DIR))
                                 path-separator
                                 (getenv "PATH")))
          (add-to-list 'exec-path (concat DIRNAME DIR))))
      (dolist (DIR '("/usr/share/man" "/share/man" "/usr/local/man" "/local/man"))
        (when (file-directory-p (concat DIRNAME DIR))
          (add-to-list 'woman-manpath (concat DIRNAME DIR))))
      (dolist (DIR '("/usr/share/info" "/share/info" "/usr/local/info" "/local/info"))
        (when (file-directory-p (concat DIRNAME DIR))
          (add-to-list 'Info-additional-directory-list (concat DIRNAME DIR))))))

  ;; root directories are added in the beginning
  (add-unix-root-dir "c:/msys64/mingw64")
  (add-unix-root-dir "c:/msys64")

  ;; Add windows_bin to PATH and exec-path
  (setenv "PATH" (concat (w32-convert-filename
                          (expand-file-name
                           (concat user-emacs-directory "windows_bin/")))
                         path-separator
                         (getenv "PATH")))
  (add-to-list 'exec-path (concat user-emacs-directory "windows_bin/"))

  ;; nodejs
  (when (file-exists-p "c:/Program Files/nodejs/nodevars.bat")
    (setq explicit-cmdproxy.exe-args '("/k \"\"C:\\Program Files\\nodejs\\nodevars.bat\"\"")))

  ;; this is a very recommended setup
  (setq w32-pipe-read-delay 0)


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
