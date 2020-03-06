;; Add support to MS Visual Studio
;;
;; References:
;; - https://walbourn.github.io/a-brief-history-of-windows-sdks/
;; - https://en.wikipedia.org/wiki/Microsoft_Windows_SDK
;; - https://en.wikipedia.org/wiki/Microsoft_Visual_Studio
;; TODO use Common7\Tools\vsdevcmd.bat to compile
(when (eq system-type 'windows-nt)
  (require 'cl)

  (defvar msvs-cpp-project-regexp ".*vcxproj$")
  (defvar msvs-cs-project-regexp ".*csproj$")
  (defvar msvs-project-regexp ".*\\(vcx\\|cs\\)proj$")
  (defvar msvs-solution-regexp ".*sln$")

  (defvar msvs-vswhere
    (concat (expand-file-name (getenv "ProgramFiles(x86)"))
            "/Microsoft Visual Studio/Installer/vswhere.exe"))

  (defvar msvs-root-directory
    (if (file-readable-p msvs-vswhere)
        (expand-file-name
         (car (process-lines msvs-vswhere "-legacy" "-latest" "-property" "installationPath")))
      (directory-parent
       (expand-file-name
        (some (lambda (ENV_VAR) (getenv ENV_VAR))
              '("VS140COMNTOOLS"  ; Visual Studio 2015
                ;; There is no VS130COMNTOOLS.
                "VS120COMNTOOLS"  ; Visual Studio 2013
                "VS110COMNTOOLS"  ; Visual Studio 2012
                "VS100COMNTOOLS"  ; Visual Studio 2010
                "VS90COMNTOOLS"   ; Visual Studio 2008
                "VS80COMNTOOLS"))) ; Visual Studio 2005
       2)))

  ;; TODO support to multiple visual studios
  (defvar msvs-include-directory
    (concat (expand-file-name (getenv "ProgramFiles(x86)"))
            "/Microsoft Visual Studio 12.0/VC/include"))

  ;; TODO support to multiple sdks. Find Windows.h @ "Microsoft SDKs/Windows/" and "Windows Kits/"
  (defvar msvs-platform-sdk
    (concat (expand-file-name (getenv "ProgramFiles(x86)"))
            "/Windows Kits/8.1/Include/um"))

  ;; Add Visual Studio binary directories to PATH
  (when msvs-root-directory
    (w32-add-to-path (concat msvs-root-directory "/Common7/Tools"))
    (add-to-list 'exec-path (concat msvs-root-directory "/Common7/Tools")))


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


  ;; NuGet
  (defun nuget-restore()
    (interactive)
    (let* ((solution-file-list (locate-dominating-file-match default-directory msvs-solution-regexp))
           (solution-path (when solution-file-list
                            (file-relative-name (file-name-directory (car solution-file-list)))))
           (nuget-file (when solution-path
                         (concat solution-path ".nuget/NuGet.exe"))))
      (if solution-path
          (if (file-exists-p nuget-file)
              (message "%s"
                       (with-temp-buffer
                         (call-process nuget-file nil t nil "restore" "-SolutionDirectory" solution-path)
                         (buffer-string)))
            (error "Nuget executable not found"))
        (error "Solution file not found"))))

  ;; Create modes for solution and project files, so we can set the compile command

  (define-generic-mode sln-mode ;; MODE
    '("#")                      ;; COMMENT-LIST
    nil                         ;; KEYWORD-LIST
    nil                         ;; FONT-LOCK-LIST
    '("\\.sln\\'")              ;; AUTO-MODE-LIST
    (list 'msvs-set-compile-command
          (lambda () (local-set-key (kbd "<f9>") 'compile))
          ) ;; FUNCTION-LIST
    )

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
  (add-to-list 'cc-search-directories msvs-include-directory t)
  (add-to-list 'cc-search-directories msvs-platform-sdk t)

  ;; flycheck-clang
  (add-to-list 'flycheck-clang-include-path msvs-include-directory)
  (add-to-list 'flycheck-clang-include-path msvs-platform-sdk)

  ;; company-c-headers
  (add-to-list 'company-c-headers-path-system msvs-include-directory)
  (add-to-list 'company-c-headers-path-system msvs-platform-sdk)

  ;; gtags
  ;; (setenv "GTAGSLIBPATH" (concat msvs-include-directory ":" msvs-platform-sdk))
  )
