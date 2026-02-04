;;; early-init.el --- Emacs early init configuration -*- lexical-binding:t -*-

;;; Code:

(defvar wsl-p (stringp (and (eq system-type 'gnu/linux)
                            (or (getenv "WSLENV")
                                (getenv "WSL_INTEROP"))))
  "Non-nil if Emacs is running on Windows Subsystem for Linux (WSL).")

;; Emacs reads your main init file after creating the initial frame, so setting there won’t have the
;; expected effect on initial frame settings.
(setopt default-frame-alist `((fullscreen . maximized)
                              (visibility . nil)
                              ,(when (and (eq system-type 'gnu/linux)
                                          (not wsl-p))
                                 '(alpha . 96))))

;; No need to waste precious desktop space with useless GUI elements
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; early-init.el ends here
