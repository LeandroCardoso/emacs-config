(when (eq system-type 'windows-nt)
  (require 'w32-extra)
  ;; root directories are added in the beginning
  (w32-add-unix-root-path "c:/msys64/mingw64")
  (w32-add-unix-root-path "c:/msys64")

  ;; Add external utilities to PATH and exec-path
  (dolist (path (list "C:/Program Files/Git/cmd/"
                      (expand-file-name "nuget/" user-emacs-directory)
                      (expand-file-name "omnisharp/" user-emacs-directory)
                      (expand-file-name "windows_bin/" user-emacs-directory)))
    (w32-add-to-path path)
    (add-to-list 'exec-path path))

  ;; Required to enter password for git
  (setenv "SSH_ASKPASS" "c:/Program Files (x86)/GitExtensions/GitExtSshAskPass.exe")
  ;; map AltGr to Alt using AutoHotKey
  (if (executable-find "altgr2alt")
      (progn
        (message "Starting altgr2alt")
        (start-process "altgr2alt" (messages-buffer) "altgr2alt")
        nil)
    (message "Error: altgr2alt not found!"))

  ;; nodejs
  (when (file-exists-p "c:/Program Files/nodejs/nodevars.bat")
    (setq explicit-cmdproxy.exe-args '("/k \"\"C:\\Program Files\\nodejs\\nodevars.bat\"\""))))
