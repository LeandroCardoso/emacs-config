;;; early-init.el --- Emacs early init configuration -*- lexical-binding:t -*-

;;; Code:

;; Emacs reads your main init file after creating the initial frame, so setting there won’t have the
;; expected effect on initial frame settings.
(setopt default-frame-alist '((fullscreen . maximized)))
(setopt initial-frame-alist (nconc default-frame-alist (list '(visibility . nil))))
(setopt window-system-default-frame-alist '((x . ((alpha . 96)))
                                            (pgtk . ((alpha . 96)))))

(add-hook 'after-init-hook 'make-frame-visible)

;; No need to waste precious desktop space with useless GUI elements
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; early-init.el ends here
