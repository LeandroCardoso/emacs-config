;; Add support to MS Visual Studio
;;
;; References:
;; - https://blogs.msdn.microsoft.com/chuckw/2013/10/03/a-brief-history-of-windows-sdks
;; - https://en.wikipedia.org/wiki/Microsoft_Windows_SDK
;; - https://en.wikipedia.org/wiki/Microsoft_Visual_Studio

(when (eq system-type 'windows-nt)
  (require 'cl)

  (defvar msvs-cpp-project-regexp ".*vcxproj$")
  (defvar msvs-cs-project-regexp ".*csproj$")
  (defvar msvs-project-regexp ".*\\(vcx\\|cs\\)proj$")
  (defvar msvs-solution-regexp ".*sln$")
  (defvar msvs-vswhere (expand-file-name (concat (getenv "ProgramFiles(x86)")
                                                 "/Microsoft Visual Studio/Installer/vswhere.exe")))


  (defun msvs-compile-command (&optional solution platform configuration target)
    "Return a `compile-command' for compile a msvs project/file."
    (let* (;; If the current buffer is a solution file then use it as solution-file, else look up
           ;; the directory hierarchy for a directory containing a solution file.
           (solution-file
            (if (string-match-p msvs-solution-regexp (or buffer-file-name ""))
                buffer-file-name
              (car (locate-dominating-file-match default-directory msvs-solution-regexp))))
           (solution-directory (if solution-file
                                   (file-name-directory solution-file)))
           ;; If the current buffer is a project file then use it as project-file, else look up the
           ;; directory hierarchy for a directory containing a project file.
           (project-file
            (if (string-match-p msvs-project-regexp (or buffer-file-name ""))
                buffer-file-name
              (car (locate-dominating-file-match default-directory msvs-project-regexp))))
           (project-directory (if project-file
                                  (file-name-directory project-file))))
      (concat "msbuild.cmd"
              (when (and (not solution)
                         solution-directory)
                (concat " /p:SolutionDir=" (w32-convert-filename solution-directory)))
              (when platform (concat " /p:Platform=" platform))
              (when configuration (concat " /p:Configuration=" configuration))
              (when target (concat " /t:" target))
              " "
              (let ((object (if solution
                                solution-file
                              (when project-file (file-relative-name project-file)))))
                (when object (w32-convert-filename object))))))


  (defun msvs-set-compile-command ()
    "Set a `compile-command' for compile a msvs project/file."
    (interactive)
    (setq-local compile-command
                (cond
                 ;; c or c++
                 ((or (eq major-mode 'c-mode)
                      (eq major-mode 'c++-mode)
                      (string-match-p msvs-cpp-project-regexp (or buffer-file-name "")))
                  (msvs-compile-command nil "win32" "Debug" "Build"))
                 ;; c#
                 ((or (eq major-mode 'csharp-mode)
                      (string-match-p msvs-cs-project-regexp (or buffer-file-name "")))
                  (msvs-compile-command t "\"Any CPU\"" "Debug" "Build"))
                 ;; solution
                 ((string-match-p msvs-solution-regexp (or buffer-file-name ""))
                  (msvs-compile-command t "win32" "Debug" "Build"))))
    ;; If the project directory is different than the default-directory then compilation-search-path
    ;; needs to be set.
    (let ((project-file (car (locate-dominating-file-match default-directory msvs-project-regexp))))
      (when project-file
        (add-to-list (make-local-variable 'compilation-search-path)
                     (file-name-directory project-file)))))

  (defun msvs-list-installations()
    "Returns a list of Microsoft Visual Studio installation versions"
    (when (file-readable-p msvs-vswhere)
      (split-string (with-output-to-string
                      (call-process msvs-vswhere nil `(,standard-output nil) nil
                                    "-legacy" "-property" "installationVersion"))
                    "[\n\r]+" t)))


  (defun msvs-root-dir ()
    "Try to detect the newest Microsoft Visual Studio installed and return its root directory.
Versions supported are from Visual Studio 2005 (8.0) up to Visual Studio 2015 (14.0)."
    (directory-parent
     (some (lambda (ENV_VAR) (getenv ENV_VAR))
           '("VS140COMNTOOLS" ; Visual Studio 2015
             ;; There is no VS130COMNTOOLS.
             "VS120COMNTOOLS" ; Visual Studio 2013
             "VS110COMNTOOLS" ; Visual Studio 2012
             "VS100COMNTOOLS" ; Visual Studio 2010
             "VS90COMNTOOLS"  ; Visual Studio 2008
             "VS80COMNTOOLS"  ; Visual Studio 2005
             ))
     2))


  ;; Add Visual Studio binary directories to PATH
  (let ((msvs-root (msvs-root-dir)))
    (when msvs-root
      (setenv "PATH"
              (concat
               (w32-convert-filename (concat msvs-root "Common7\\Tools"))
               path-separator
               (w32-convert-filename (concat msvs-root "Common7\\IDE"))
               path-separator
               (getenv "PATH")))))

  (add-to-list 'auto-mode-alist '("\\.rc\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.inf\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("msbuild[0-9]*\\.log\\'" . compilation-mode))

  ;; An ungly hack to idenfity c++ extensionless files as c++ file. Thanks ISO c++, a file without
  ;; extension was a great idea!
  (add-to-list 'auto-mode-alist '("[Ii]nclude" . c++-mode) t)

  (with-eval-after-load "grep"
    (add-to-list 'grep-files-aliases
                 '("msvs" . "*.sln *proj *proj.filters *.props *.targets packages.config app.config"))

    (dolist (file '("*.pdb" ".vs"))
      (add-to-list 'grep-find-ignored-files file))
    )

  ;; set compile-command
  (add-hook 'c-mode-hook 'msvs-set-compile-command)
  (add-hook 'c++-mode-hook 'msvs-set-compile-command)
  (with-eval-after-load "csharp-mode" (add-hook 'csharp-mode-hook 'msvs-set-compile-command))

  ;; c/c++ headers
  (add-to-list 'cc-search-directories (concat (msvs-root-dir) "VC/include") t)
  ;; c/c++ ATL/MFC headers
  ;; (add-to-list 'cc-search-directories (concat (msvs-root-dir) "VC/atlmfc/include") t)


  (add-to-list 'cc-search-directories "C:/Program Files (x86)/Microsoft SDKs/Windows/v7.1A/Include" t)

    ;; gtags
  (setenv "GTAGSLIBPATH" (concat "C:\\Program Files\\Microsoft SDKs\\Windows\\v7.1A\\Include:"
                                 "C:\\Program Files\\Microsoft Visual Studio 12.0\\VC\\include"))
  )
