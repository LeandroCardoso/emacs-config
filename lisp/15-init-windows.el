(when (eq system-type 'windows-nt)
  (require 'w32-extra)
  ;; root directories are added in the beginning
  (w32-add-unix-root-path "c:/msys64/ucrt64")
  (w32-add-unix-root-path "c:/msys64")

  ;; Add external utilities to PATH and exec-path
  (dolist (path (list "C:/Program Files/Git/cmd/"
                      "C:/Program Files (x86)/Java/latest/jre-1.8/bin"
                      (expand-file-name "omnisharp/" user-emacs-directory)
                      (expand-file-name "windows_bin/" user-emacs-directory)))
    (w32-add-to-path path))

  ;; Required to enter password for git
  (setenv "SSH_ASKPASS" "c:/Program Files/Git/mingw64/bin/git-askpass.exe")

  ;; map AltGr to Alt using AutoHotKey
  (if (executable-find "altgr2alt")
      (progn
        (message "Starting altgr2alt")
        (start-process "altgr2alt" (messages-buffer) "altgr2alt")
        nil)
    (message "Error: altgr2alt not found!"))

  ;; nodejs
  (when (file-exists-p "c:/Program Files/nodejs/nodevars.bat")
    (setq explicit-cmdproxy.exe-args '("/k \"\"C:\\Program Files\\nodejs\\nodevars.bat\"\"")))

  ;; Workaround for signature error when managing elpa packages
  (require 'package)
  (setopt package-check-signature nil))
