;; -*- lexical-binding: t -*-
(when (eq system-type 'windows-nt)
  (require 'w32-extra)
  ;; root directories are added in the beginning
  (w32-add-unix-root-path "c:/msys64/mingw64")
  (w32-add-unix-root-path "c:/msys64")

  ;; Add windows_bin to PATH and exec-path
  (w32-add-to-path (expand-file-name "windows_bin/" user-emacs-directory))
  (add-to-list 'exec-path (expand-file-name "windows_bin/" user-emacs-directory))

  ;; Add Git For Windows to exec-path
  (add-to-list 'exec-path "C:/Program Files/Git/cmd")

  ;; Workaround for ssh
  (setenv "SSH_ASKPASS" "c:/msys64/usr/bin/sshpass.exe")

  ;; nodejs
  (when (file-exists-p "c:/Program Files/nodejs/nodevars.bat")
    (setq explicit-cmdproxy.exe-args '("/k \"\"C:\\Program Files\\nodejs\\nodevars.bat\"\""))))
