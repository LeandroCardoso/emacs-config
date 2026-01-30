;;; nuget.el --- NuGet support for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:
;; Simple NuGet integration for restoring packages and managing sources.

;;; Code:

(require 'files-extra)
(require 'subr-x)
(require 'view)

;; Variables

(defconst nuget-output-buffer "*nuget*"
  "Buffer name used to display NuGet command output.")

(defconst nuget-solution-regexp ".*\\.sln\\'"
  "Regexp matching solution files for NuGet operations.")

;; User Options

(defgroup nuget nil
  "NuGet support for Emacs."
  :group 'tools)

(defcustom nuget-config-file nil
  "The NuGet configuration file path.

When nil the default configuration will be used."
  :type 'file
  :group 'nuget)

;; Internal helpers

(defun nuget--ensure-executable ()
  "Signal an error if the NuGet executable is not available."
  (unless (executable-find "nuget")
    (error "NuGet executable not found")))

(defun nuget--call (&rest args)
  "Call NuGet synchronously with ARGS and return its output as a string."
  (nuget--ensure-executable)
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "nuget" nil t nil args)))
      (unless (zerop exit-code)
        (error "NuGet failed: %s" (string-trim (buffer-string))))
      (buffer-string))))

(defun nuget--start (&rest args)
  "Start NuGet asynchronously with ARGS and display its output."
  (nuget--ensure-executable)
  (let ((proc (apply #'start-process "nuget" nuget-output-buffer "nuget" args)))
    (view-buffer nuget-output-buffer)
    proc))

(defun nuget--list-sources ()
  "Return a list of configured NuGet source names."
  (let* ((output (apply #'nuget--call
                        (append '("sources" "list" "-format" "short" "-noninteractive")
                                (when nuget-config-file (list "-configfile" nuget-config-file)))))
         (lines (split-string output "\n" t)))
    (mapcar (lambda (line)
              ;; Remove leading letters if present
              (replace-regexp-in-string "^[A-Z]+ +" "" line))
            lines)))

;; Commands

;;;###autoload
(defun nuget-restore ()
  "Restore NuGet packages for the current solution."
  (interactive)
  (let* ((solutions (locate-dominating-file-match default-directory nuget-solution-regexp))
         (default-directory
           (when solutions
             (file-name-directory (car solutions)))))
    (unless solutions
      (error "Solution file not found"))
    (apply #'nuget--start
           (append '("restore" "-noninteractive")
                   (when nuget-config-file (list "-configfile" nuget-config-file))))))

;;;###autoload
(defun nuget-update-password (&optional source-name user-name password)
  "Update the password for a NuGet source.

When SOURCE-NAME, USER-NAME, or PASSWORD are nil, prompt for them."
  (interactive)
  (let* ((source-name
          (or source-name
              (completing-read "NuGet source: "
                               (nuget--list-sources)
                               nil t)))
         (user-name
          (or user-name
              (read-string "NuGet user name: ")))
         (password
          (or password
              (read-passwd "NuGet password: "))))
    (apply #'nuget--call
           (append (list "sources" "update"
                         "-name" source-name
                         "-user" user-name
                         "-pass" password
                         "-noninteractive")
                   (when nuget-config-file (list "-configfile" nuget-config-file))))))

;; Windows-only commands

(when (eq system-type 'windows-nt)

;;;###autoload
  (defun nuget-install ()
    "Download and install NuGet on Windows."
    (interactive)
    (let ((url "https://dist.nuget.org/win-x86-commandline/latest/nuget.exe")
          (default-directory (expand-file-name "windows/bin/" user-emacs-directory)))
      (unless (executable-find "curl")
        (error "Curl executable not found"))
      (make-directory default-directory t)
      (message "Downloading NuGet...")
      (start-process "curl" nuget-output-buffer "curl" "-s" "-O" url)
      (view-buffer nuget-output-buffer)))

;;;###autoload
  (defun nuget-install-update ()
    "Update the installed NuGet executable."
    (interactive)
    (nuget--start "update" "-self" "-noninteractive")))

(provide 'nuget)

;;; nuget.el ends here
