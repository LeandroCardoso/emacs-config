;;; wsl-extra.el --- Extra functions for WSL (Windows Subsystem for Linux) -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(defun wsl-wslpath-ensure-executable ()
  "Signal an error if the wslpath executable is not available."
  (unless (executable-find "wslpath")
    (error "wslpath executable not found")))

(defun wsl-convert-filename (file-name &optional to-windows)
  "Convert the FILE-NAME to something suitable for Windows or Linux.

Default is to convert a Windows path to WSL path. When optional
parameter TO-WINDOWS is non nil, convert a WSL path to a Windows path."
  (wsl-wslpath-ensure-executable)
  (with-temp-buffer
    (call-process "wslpath" nil t nil "-a" (if to-windows "-w" "-u") file-name)
    (goto-char (point-min))
    (buffer-substring (pos-bol) (pos-eol))))

(defun wsl-convert-filename-to-linux (file-name)
  "Convert the FILE-NAME from a Windows path to a WSL path."
  (wsl-convert-filename file-name))

(defun wsl-convert-filename-to-windows (file-name)
  "Convert the FILE-NAME from a WSL path to a Windows path."
  (wsl-convert-filename file-name t))

(provide 'wsl-extra)

;;; wsl-extra.el ends here
