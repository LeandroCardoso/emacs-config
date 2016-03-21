;; No need to waste precious desktop space with useless GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Frame
(setq initial-frame-alist '((fullscreen . maximized)))
(setq default-frame-alist initial-frame-alist)
(setq window-system-default-frame-alist '((x . ((alpha . 97)))))

;; Font
(cond ((eq system-type 'gnu/linux)
       (set-frame-font "Source Code Pro-11" t t))
      ((eq system-type 'windows-nt)
       (set-frame-font "Consolas 10" t t)))

;; Theme
(when (require 'monokai-theme nil t)
  (setq monokai-use-variable-pitch nil)
  (load-theme 'monokai t)
  (set-face-attribute 'cursor nil :background (face-foreground 'mode-line-buffer-id))
  (set-face-attribute 'fringe nil :foreground "dark slate gray")) ;; dim gray is also a good option

;; Faces
(set-face-attribute 'bold-italic nil :inherit '(bold italic))
(set-face-attribute 'italic nil :underline t)
(set-face-attribute 'woman-bold nil :inherit '(Man-overstrike))
(set-face-attribute 'woman-italic nil :inherit '(Man-underline))
