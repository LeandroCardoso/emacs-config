;;; nuget.el --- Add support to nuget -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:
(require 'files-extra)
(require 'view)

(defconst msvs-nuget-buffer "*nuget*")

(defun nuget-execute (&rest program-args)
  "Start a nuget subprocess. The arguments PROGRAM-ARGS are strings
to give nuget as arguments."
  (unless (executable-find "nuget")
    (error "Nuget executable not found"))
  (apply 'start-process "nuget" msvs-nuget-buffer "nuget" program-args)
  (view-buffer msvs-nuget-buffer))

;;;###autoload
(defun nuget-install ()
  "Download and install nuget."
  (interactive)
  (let ((url "https://dist.nuget.org/win-x86-commandline/latest/nuget.exe")
        (default-directory (expand-file-name "windows/bin/" user-emacs-directory)))
    (message "Downloading nuget")
    (unless (executable-find "curl")
      (error "Curl executable not found"))
    (unless (start-process "curl" (messages-buffer) "curl" "-s" "-O" url)
      (message "Error: Nuget could not be downloaded"))))

;;;###autoload
(defun nuget-restore()
  "Restore nuget packages for the current solution."
  (interactive)
  (let* ((solution-file-list (locate-dominating-file-match default-directory msvs-solution-regexp))
         (default-directory (when solution-file-list
                              (file-name-directory (car solution-file-list)))))
    (unless solution-file-list
      (error "Solution file not found"))
    (nuget-execute "restore" "-NonInteractive")))

;;;###autoload
(defun nuget-update ()
  "Update the installed nuget."
  (interactive)
  (nuget-execute "update" "-self"))

(provide 'nuget)

;; nuget.el ends here
