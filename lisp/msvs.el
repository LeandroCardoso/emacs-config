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

;; Custom

(defgroup msvs nil
  "Ms Visual Studio integration"
  :group 'tools)

(defcustom msvs-compile-default-parameters '("/m" "/v:minimal" "/fl" "/flp:verbosity=minimal")
  "Default parameters for msbuild used by `msvs-generate-compile-command'.

See https://docs.microsoft.com/en-us/visualstudio/msbuild/msbuild-command-line-reference?view=vs-2022."
  :type '(repeat (string :tag "Parameter"))
  :group 'msvs)

(defcustom msvs-compile-command-function 'msvs-compile-command-default-function
  "Function called to generate a compilation command for MSVS solution,
project or file.

See the helper function `msvs-generate-compile-command' and user option
`msvs-compile-default-parameters'."
  :type 'function
  :group 'msvs)

(defcustom msvs-convert-filename-function 'msvs-convert-filename-default-function
  "Function called to convert a filename to something suitable for msbuild."
  :type 'function
  :group 'msvs)


;; Constants

(defconst msvs-cpp-project-regexp ".*vcxproj$")
(defconst msvs-cs-project-regexp ".*csproj$")
(defconst msvs-all-projects-regexp ".*\\(vcx\\|cs\\)proj$")
(defconst msvs-solution-regexp ".*sln$")


;; Functions

(defun msvs-compile-command-default-function ()
  "Default function to generate a compilation command for msvs
solution, project or file.

See `msvs-compile-command-function'."
  (msvs-generate-compile-command t "\"Mixed Platforms\"" "Debug" "Build"))

(defun msvs-convert-filename-default-function (file-name)
  "Default function to convert FILE-NAME to something suitable for msbuild.

This default implementation returns FILE-NAME unmodified.

See `msvs-convert-filename-function'."
  file-name)

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
            (when msvs-compile-default-parameters
              (concat " " (string-join msvs-compile-default-parameters " ")))
            (when rest-parameters
              (concat " " (string-join rest-parameters " ")))
            (when (and useProjectFile solution-directory)
              (concat " /p:SolutionDir=" (funcall msvs-convert-filename-function solution-directory)))
            (when platform
              (concat " /p:Platform=" platform))
            " /p:Configuration=" configuration
            " /t:" target
            (when comp-object
              (concat " " (funcall msvs-convert-filename-function comp-object))))))

(defun msvs-set-compile-command ()
  "Set a `compile-command' for compile a msvs solution, project or file.

The function defined in `msvs-compile-command-function' is used to
generate a compilation command."
  (interactive)
  (when-let ((command (funcall msvs-compile-command-function)))
    (setq-local compile-command command)
    ;; If the project directory is different than the default-directory then
    ;; compilation-search-path needs to be set.
    (let ((project-file (car (locate-dominating-file-match default-directory msvs-all-projects-regexp))))
      (when project-file
        (add-to-list (make-local-variable 'compilation-search-path)
                     (file-name-directory project-file))))))


;; Integration

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
  (add-hook 'c++-mode-hook 'msvs-set-compile-command)
  (add-hook 'c-ts-base-mode-hook 'msvs-set-compile-command))
(with-eval-after-load "csharp-mode"
  (add-hook 'csharp-mode-hook 'msvs-set-compile-command)
  (add-hook 'csharp-ts-mode-hook 'msvs-set-compile-command))


;; Modes
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

;; An ugly hack to identify c++ extensionless files as c++ file. Thanks ISO c++, a file without
;; extension was a great idea!
(add-to-list 'auto-mode-alist '("[Ii]nclude" . c++-mode) t)

(provide 'msvs)

;;; msvs.el ends here
