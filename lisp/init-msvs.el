;; Add support to MS Visual Studio
;;
;; References:
;; - https://blogs.msdn.microsoft.com/chuckw/2013/10/03/a-brief-history-of-windows-sdks
;; - https://en.wikipedia.org/wiki/Microsoft_Windows_SDK
;; - https://en.wikipedia.org/wiki/Microsoft_Visual_Studio

(when (eq system-type 'windows-nt)
  (require 'cl)


  (defun msvs-root-dir ()
    "Try to detect the newest Microsoft Visual Studio installed and return its root directory.
Versions supported are from Visual Studio 2005 (8.0) up to Visual Studio 2015 (14.0)."
    (directory-parent
     (some (lambda (ENV_VAR) (getenv ENV_VAR))
           '("VS150COMNTOOLS" ; Visual Studio 2017
             "VS140COMNTOOLS" ; Visual Studio 2015
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
               (convert-standard-filename (concat msvs-root "Common7\\Tools"))
               path-separator
               (convert-standard-filename (concat msvs-root "Common7\\IDE"))
               path-separator
               (getenv "PATH")))))


  (require 'semantic)
  (let ((msvs-root (msvs-root-dir)))
    (when msvs-root
      ;; TODO Window SDKs
      ;; C:\Program Files\Microsoft SDKs\Windows\v6.0A\Include
      ;; C:\Program Files\Microsoft SDKs\Windows\v7.0A\Include
      ;; C:\Program Files\Microsoft SDKs\Windows\v7.1A\Include
      ;; C:\Program Files\Windows Kits\8.0\Include\shared
      ;; C:\Program Files\Windows Kits\8.0\Include\um
      ;; C:\Program Files\Windows Kits\8.1\Include\shared
      ;; C:\Program Files\Windows Kits\8.1\Include\um
      ;; (semantic-add-system-include "C:/Program Files/Microsoft SDKs/Windows/v7.1A/Include" 'c-mode)
      ;; (semantic-add-system-include "C:/Program Files/Microsoft SDKs/Windows/v7.1A/Include" 'c++-mode)
      (semantic-add-system-include (concat msvs-root "VC/include") 'c-mode)
      (semantic-add-system-include (concat msvs-root "VC/include") 'c++-mode)))


  (add-to-list 'auto-mode-alist '("\\.rc\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.inf\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("msbuild[0-9]*\\.log\\'" . compilation-mode))
  (add-to-list 'auto-mode-alist '("\\.proj\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.vcxproj\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.vcxproj\\.filters\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.props\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.targets\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))

  ;; An ungly hack to idenfity c++ extensionless files as c++ file. Thanks ISO c++, a file without
  ;; extension was a great idea!
  (add-to-list 'auto-mode-alist '("[Ii]nclude" . c++-mode) t)

  (with-eval-after-load "grep"
    ;; MSVS
    (add-to-list 'grep-files-aliases '("msvs" . "*.sln *proj *proj.filters *.props *.targets")))

  (setq compile-command "msbuild.cmd /p:Platform=win32 /p:Configuration=Debug /t:Build")

  ;; c/c++ headers
  (add-to-list 'cc-search-directories (concat (msvs-root-dir) "VC/include") t)
  ;; c/c++ ATL/MFC headers
  ;; (add-to-list 'cc-search-directories (concat (msvs-root-dir) "VC/atlmfc/include") t)


  (add-to-list 'cc-search-directories "C:\\Program Files\\Microsoft SDKs\\Windows\\v7.1A\\Include" t)
  )
