;;; early-init.el --- Emacs early init configuration -*- lexical-binding:t -*-

;;; Code:

;; Emacs reads your main init file after creating the initial frame, so setting there won’t have the
;; expected effect.
(setopt default-frame-alist '((fullscreen . maximized) (vertical-scroll-bars . nil)))
(setopt initial-frame-alist default-frame-alist)
(setopt window-system-default-frame-alist '((x . ((alpha . 96)))
                                            (pgtk . ((alpha . 96)))))

;; No need to waste precious desktop space with useless GUI elements
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; early-init.el ends here
