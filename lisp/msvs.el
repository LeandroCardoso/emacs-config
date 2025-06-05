;;; msvs.el --- Add support to MS Visual Studio -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

;; References:

;; - https://walbourn.github.io/a-brief-history-of-windows-sdks/
;; - https://en.wikipedia.org/wiki/Microsoft_Windows_SDK
;; - https://en.wikipedia.org/wiki/Microsoft_Visual_Studio
;; TODO use Common7\Tools\vsdevcmd.bat to compile

(require 'files-extra)
(require 'w32-extra)

;; Custom

(defgroup msvs nil
  "Ms Visual Studio integration"
  :group 'tools)

(defcustom msvs-msbuild-default-parameters '("/m" "/v:minimal" "/fl" "/flp:verbosity=minimal")
  "Default parameters for msbuild used by `msvs-generate-compile-command'.

See https://docs.microsoft.com/en-us/visualstudio/msbuild/msbuild-command-line-reference?view=vs-2022.")

(defcustom msvs-compile-command-function nil
  "Function called to generate a compilation command for msvs
solution, project or file.  When unset or the return of the
called function is nil, then
`msvs-compile-command-default-function' is used.

See the helper function `msvs-generate-compile-command' and user
option `msvs-msbuild-default-parameters'.")


;; Constants

(defconst msvs-cpp-project-regexp ".*vcxproj$")
(defconst msvs-cs-project-regexp ".*csproj$")
(defconst msvs-all-projects-regexp ".*\\(vcx\\|cs\\)proj$")
(defconst msvs-solution-regexp ".*sln$")

(defconst msvs-program-files
  (when (getenv "ProgramFiles(x86)")
    (concat (expand-file-name (getenv "ProgramFiles(x86)")) "/")))

(defconst msvs-vswhere
  (when msvs-program-files
    (concat msvs-program-files "Microsoft Visual Studio/Installer/vswhere.exe")))

(defconst msvs-root-directory
  (if (and msvs-vswhere (file-readable-p msvs-vswhere))
      (expand-file-name
       (car (process-lines msvs-vswhere "-legacy" "-latest" "-property" "installationPath")))
    (let ((vscomntools (seq-some (lambda (ENV_VAR) (getenv ENV_VAR))
                                 '("VS140COMNTOOLS"    ; Visual Studio 2015
                                   ;; There is no VS130COMNTOOLS.
                                   "VS120COMNTOOLS"    ; Visual Studio 2013
                                   "VS110COMNTOOLS"    ; Visual Studio 2012
                                   "VS100COMNTOOLS"    ; Visual Studio 2010
                                   "VS90COMNTOOLS"     ; Visual Studio 2008
                                   "VS80COMNTOOLS")))) ; Visual Studio 2005
      (when vscomntools
        (directory-parent (expand-file-name vscomntools) 2)))))

;; TODO support to multiple visual studios
(defconst msvs-include-directory
  (when msvs-program-files
    (concat msvs-program-files "Microsoft Visual Studio 12.0/VC/include")))

;; TODO support to multiple sdks. Find Windows.h @ "Microsoft SDKs/Windows/" and "Windows Kits/"
(defconst msvs-platform-sdk
  (when msvs-program-files
    (concat msvs-program-files "Windows Kits/8.1/Include/um")))

(defconst msvs-nuget-buffer "*nuget*")


;; Functions

(defun msvs-generate-compile-command (useProjectFile platform configuration target &rest rest-parameters)
  "Return a string for compile a msvs solution, project or file."
  (require 'subr-x)  
  (let* (
         ;; Ignore useProjectFile parameter when the current buffer is a solution file.
         (useProjectFile (and useProjectFile
                              (not (string-match-p msvs-solution-regexp (or buffer-file-name "")))))
         ;; If the current buffer is a project file, then use it as project-file, else look up in
         ;; the directory hierarchy for a directory containing a project file.
         (project-file (if (string-match-p msvs-all-projects-regexp (or buffer-file-name ""))
                           (file-local-name buffer-file-name)
                         (car (locate-dominating-file-match default-directory msvs-all-projects-regexp))))
         (project-directory (when project-file
                              (file-name-directory project-file)))
         ;; If the current buffer is a solution file, then use it as solution-file, else look up in
         ;; the directory hierarchy for a directory containing a solution file.
         (solution-file (if (string-match-p msvs-solution-regexp (or buffer-file-name ""))
                            (file-local-name buffer-file-name)
                          (car (locate-dominating-file-match default-directory msvs-solution-regexp))))
         (solution-directory (when solution-file
                               (file-name-directory solution-file)))
         (comp-object (if useProjectFile
                          (when project-file
                            (file-relative-name project-file))
                        solution-file)))
    (concat "msbuild.cmd"
            (when msvs-msbuild-default-parameters
              (concat " " (string-join msvs-msbuild-default-parameters " ")))
            (when rest-parameters
              (concat " " (string-join rest-parameters " ")))
            (when (and useProjectFile solution-directory)
              (concat " /p:SolutionDir=" (w32-convert-filename solution-directory)))
            (when platform
              (concat " /p:Platform=" platform))
            " /p:Configuration=" configuration
            " /t:" target
            (when comp-object
              (concat " " (w32-convert-filename comp-object))))))

(defun msvs-compile-command-default-function ()
  "Default function to generate a compilation command for msvs
solution, project or file."
  (msvs-generate-compile-command t "\"Mixed Platforms\"" "Debug" "Build"))

(defun msvs-set-compile-command ()
  "Set a `compile-command' for compile a msvs solution, project or file.

The function defined in `msvs-compile-command-function' is used
to generate a compilation command. If it is not defined, or it
returs nil, then the `msvs-compile-command-default-function' is
used."
  (interactive)
  (when msvs-root-directory
    (when-let ((command (or (when msvs-compile-command-function
                              (funcall msvs-compile-command-function))
                            (msvs-compile-command-default-function))))
      (setq-local compile-command command)
      ;; If the project directory is different than the default-directory then
      ;; compilation-search-path needs to be set.
      (let ((project-file (car (locate-dominating-file-match default-directory msvs-all-projects-regexp))))
        (when project-file
          (add-to-list (make-local-variable 'compilation-search-path)
                       (file-name-directory project-file)))))))

;; NuGet
(defun nuget-execute (&rest program-args)
  "Start a nuget subprocess. The arguments PROGRAM-ARGS are strings
to give nuget as arguments."
  (require 'view)
  (unless (executable-find "nuget")
    (error "Nuget executable not found"))
  (apply 'start-process "nuget" msvs-nuget-buffer "nuget" program-args)
  (view-buffer msvs-nuget-buffer))

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

(defun nuget-update ()
  "Update the installed nuget."
  (interactive)
  (nuget-execute "update" "-self"))

(defun nuget-restore()
  "Restore nuget packages for the current solution."
  (interactive)
  (let* ((solution-file-list (locate-dominating-file-match default-directory msvs-solution-regexp))
         (default-directory (when solution-file-list
                              (file-name-directory (car solution-file-list)))))
    (unless solution-file-list
      (error "Solution file not found"))
    (nuget-execute "restore" "-NonInteractive")))


;; Setup

;; Add Visual Studio to PATH
(when msvs-root-directory
  ;; Add Visual Studio binary directories to PATH
  (w32-add-to-path (concat msvs-root-directory "/Common7/Tools")))

;; c/c++ fast header/implementation switch
(with-eval-after-load "find-file"
  (when msvs-include-directory
    (add-to-list 'cc-search-directories msvs-include-directory t))
  (when msvs-platform-sdk
    (add-to-list 'cc-search-directories msvs-platform-sdk t)))

;; flycheck-clang
(with-eval-after-load "flycheck-clang"
  (when msvs-include-directory
    (add-to-list 'flycheck-clang-include-path msvs-include-directory))
  (when msvs-platform-sdk
    (add-to-list 'flycheck-clang-include-path msvs-platform-sdk)))

;; company-c-headers
(with-eval-after-load "company"
  (when msvs-include-directory
    (add-to-list 'company-c-headers-path-system msvs-include-directory))
  (when msvs-platform-sdk
    (add-to-list 'company-c-headers-path-system msvs-platform-sdk)))

;; project
(with-eval-after-load "project"
  (add-to-list 'project-vc-extra-root-markers "*.sln"))

;; grep
(with-eval-after-load "grep"
  (add-to-list 'grep-files-aliases
               '("msvs" . "*.nuspec *.props *.sln *.targets *proj *proj.filters app.config packages.config"))
  (dolist (file '("*.pdb" ".vs"))
    (add-to-list 'grep-find-ignored-files file)))

;; compile
(with-eval-after-load "cc-mode"
  (add-hook 'c-mode-hook 'msvs-set-compile-command)
  (add-hook 'c++-mode-hook 'msvs-set-compile-command))
(with-eval-after-load "csharp-mode"
  (add-hook 'csharp-mode-hook 'msvs-set-compile-command))


;; Setup Auto-Modes
(require 'derived)
(require 'generic)

;; Create modes for solution and project files, so we can set the compile command
(define-generic-mode sln-mode                      ; MODE
  '("#")                                           ; COMMENT-LIST
  nil                                              ; KEYWORD-LIST
  nil                                              ; FONT-LOCK-LIST
  '("\\.sln\\'")                                   ; AUTO-MODE-LIST
  (list 'msvs-set-compile-command
        (lambda ()
          (local-set-key (kbd "<f9>") 'compile)))) ; FUNCTION-LIST

(define-derived-mode vsproj-mode nxml-mode "VS-proj" nil
  (msvs-set-compile-command)
  (local-set-key (kbd "<f9>") 'compile))

(add-to-list 'auto-mode-alist '("\\.rc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.inf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("msbuild[0-9]*\\.log\\'" . compilation-mode))
(add-to-list 'auto-mode-alist '("\\.\\(vcx\\|cs\\)proj\\'" . vsproj-mode))

;; An ungly hack to idenfity c++ extensionless files as c++ file. Thanks ISO c++, a file without
;; extension was a great idea!
(add-to-list 'auto-mode-alist '("[Ii]nclude" . c++-mode) t)

(provide 'msvs)

;; msvs.el ends here
