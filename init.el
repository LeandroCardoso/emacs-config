;;; init.el --- Emacs configuration -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso <leandrocardoso@gmail.com>

;;; Commentary:

;;; Code:

;; Initialization

(setopt custom-file (concat user-emacs-directory "custom-variables.el"))
(setopt gc-cons-threshold (* 32 1024 1024))

;; We must require the 'use-package' at the beginning, so the `use-package-verbose' works properly
(require 'use-package)

;; Emacs packages

(use-package emacs
  :config
  (setopt frame-resize-pixelwise t)
  (setopt highlight-nonselected-windows t)
  (setopt inhibit-startup-screen t)
  (setopt initial-scratch-message nil)
  (setopt ring-bell-function 'ignore)
  (setopt sentence-end-double-space nil)
  (setopt use-short-answers t)

  ;; Hack to set the major mode automatically with new buffers not associated with a file
  ;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
  (setq-default major-mode
                (lambda () (if buffer-file-name
                               (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))
  :bind
  ("<escape>" . execute-extended-command))

(use-package autorevert
  :config
  (setopt auto-revert-verbose nil)
  (global-auto-revert-mode))

(use-package calc
  :defer t
  :config
  (setopt calc-multiplication-has-precedence nil))

(use-package calendar
  :defer t
  :config
  (calendar-set-date-style 'european))

(use-package cc-mode
  :defer t
  :config
  (setopt c-guess-region-max 100000)

  (c-add-style "c++-custom" '("stroustrup" (c-offsets-alist (inlambda . 0) (inline-open . 0))))
  (add-to-list 'c-default-style '(c++-mode . "c++-custom"))
  (add-to-list 'c-default-style '(c-mode . "c++-custom"))

  (add-to-list 'c-font-lock-extra-types "BOOL")
  (add-to-list 'c++-font-lock-extra-types "BOOL")

  (defun c-setup ()
    (c-toggle-comment-style -1))

  (defun c++-setup ())

  (defun c-c++-setup ()
    (font-lock-add-keywords nil '(("\\<\\(TRUE\\|FALSE\\)\\>" . 'font-lock-constant-face))))
  
  :hook
  (c-mode . c-setup)
  (c-mode . c-c++-setup)
  (c++-mode . c++-setup)
  (c++-mode . c-c++-setup))

(use-package compile
  :defer t
  :config
  (setopt compilation-scroll-output 'first-error)
  (setopt compilation-error-screen-columns nil)

  :bind
  (:map prog-mode-map
        ("<f9>" . compile)))

(use-package desktop
  :defer t
  :config
  (defun desktop-save-mode-on ()
    "Enable `desktop-save-mode'.  Provided for use in hooks."
    (desktop-save-mode 1))

  (setopt desktop-save 'ask-if-exists)
  (add-to-list 'desktop-locals-to-save 'buffer-display-time)

  :hook
  (desktop-after-read . clean-buffer-list)
  ;; I am enabling the desktop-save-mode *after* a desktop session is loaded by the `desktop-read'
  ;; command to avoid loading a desktop session on Emacs initialization
  (desktop-after-read . desktop-save-mode-on))

(use-package eglot
  :hook
  (csharp-mode . eglot-ensure))

(use-package eldoc
  :config
  (setopt eldoc-echo-area-use-multiline-p t)
  (global-eldoc-mode))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package elisp-mode
  :defer t
  :config
  (require 'mode-local)
  (setq-mode-local emacs-lisp-mode sentence-end-double-space t)

  :bind
  (:map emacs-lisp-mode-map
        ;; eval-defun is also in C-M-x)
        ("C-c C-c" . eval-defun)))

(use-package find-file
  :defer t
  :config
  (setopt cc-search-directories '("." "./*" "../*" "/usr/include" "/usr/local/include/*")))

(use-package goto-addr
  :config
  (global-goto-address-mode))

(use-package help
  :defer t
  :config
  (setopt describe-bindings-outline t)
  (setopt help-enable-symbol-autoload t))

(use-package hi-lock
  :config
  (global-hi-lock-mode))

(use-package hi-line
  :config
  ;; hl-line-mode causes slowness when scrolling down repeatedly, this is a workaround for it
  (setq auto-window-vscroll nil)
  :hook
  ((archive-mode
    dashboard-mode
    dired-mode
    grep-mode
    ibuffer-mode
    occur-mode
    proced-mode
    tabulated-list-mode
    tar-mode) . hl-line-mode))

(use-package mule
  :config
  ;; Set coding system to utf-8
  (set-language-environment "UTF-8")

  ;; Force unix EOL in emacs-lisp files outside of unix
  (modify-coding-system-alist 'file "\\.el\\'" 'prefer-utf-8-unix))

(use-package novice
  :defer t
  :config
  (setq disabled-command-function nil))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa stable" . "https://stable.melpa.org/packages/"))

  (setopt package-archive-priorities
          '(("melpa"        . 3)
            ("nongnu"       . 2)
            ("gnu"          . 1)
            ("melpa stable" . 0)))
  :bind
  ;; original is mark-page
  ("C-x C-p" . list-packages))

(use-package shortdoc
  :defer t
  :config
  (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function))

(use-package simple
  :config
  (setopt eval-expression-print-length nil)
  (setopt goto-line-history-local t)
  (setopt next-error-message-highlight t)
  (setopt shift-select-mode nil)
  (setopt what-cursor-show-names t))

(use-package select
  :config
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setopt x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package so-long
  :config
  (global-so-long-mode))

(use-package use-package
  :config
  (setopt use-package-enable-imenu-support t)
  (setopt use-package-verbose t))

;; External packages

(use-package amx
  :ensure t
  :config
  (setopt amx-prompt-string "> ")
  (amx-mode))

(use-package avy
  :ensure t
  :defer t
  :config
  (setopt avy-all-windows nil)
  (setopt avy-dispatch-alist nil)
  (setopt avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/))
  (setopt avy-subword-extra-word-chars nil)
  :bind
  ("C-M-;" . avy-goto-word-1)
  ("C-<dead-acute>" . avy-goto-word-1)
  ("C-M-:" . avy-goto-char-in-line))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (company-tng-mode)

  (setopt company-format-margin-function 'company-text-icons-margin)

  ;; dabbrev
  (setopt company-dabbrev-char-regexp "\\sw\\|_\\|-")
  (setopt company-dabbrev-downcase nil)

  ;; dabbrev code
  (setopt company-dabbrev-code-everywhere t)

  :bind
  (:map prog-mode-map
        ("C-<tab>" . company-indent-or-complete-common))
  (:map text-mode-map
        ("C-<tab>" . company-indent-or-complete-common))
  (:map company-active-map
        ("<escape>" . company-abort)
        ("<next>" . company-next-page)
        ("C-v" . company-next-page)
        ("<prior>" . company-previous-page)
        ("M-v" . company-previous-page))
  (:map company-search-map
        ("<escape>" . company-search-abort)))

(use-package company-flx
  :ensure t
  :config
  (company-flx-mode +1))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package dashboard
  :ensure t
  :init
  (setopt dashboard-icon-type (if (eq system-type 'windows-nt)
                                  nil
                                'nerd-icons))

  :config
  (require 'dashboard-desktop)
  (setopt dashboard-items '((desktop . 5)
                            (recents . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5)))
  (setopt dashboard-set-file-icons t)
  (setopt dashboard-set-heading-icons t)
  (setopt dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook)

  :hook
  (dashboard-after-initialize . dashboard-jump-to-desktop))

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)
  (load (expand-file-name "engine-mode/config" user-emacs-directory))
  (load (expand-file-name "engine-mode/config-rdi" user-emacs-directory)))

(use-package flycheck
  :ensure t
  :config
  (setopt flycheck-check-syntax-automatically (if (eq system-type 'windows-nt)
                                                  '(save idle-change idle-buffer-switch)
                                                '(save idle-change mode-enabled)))
  (setopt flycheck-checker-error-threshold nil)
  (setopt flycheck-global-modes '(c-mode
                                  c++-mode
                                  csharp-mode
                                  emacs-lisp-mode
                                  js-mode
                                  typescript-mode))
  (setopt flycheck-idle-buffer-switch-delay (if (eq system-type 'windows-nt) 30 5))
  (setopt flycheck-idle-change-delay (if (eq system-type 'windows-nt) 30 5))

  ;; clang
  (flycheck-add-next-checker 'c/c++-clang 'c/c++-cppcheck)

  ;; cppcheck
  ;; unusedStructMember is annoying in header files
  (setopt flycheck-cppcheck-suppressions '("unusedStructMember"))

  (global-flycheck-mode))

(use-package goto-chg
  :ensure t
  :bind
  ("C-." . goto-last-change)
  ("C-," . goto-last-change-reverse))

(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode)
  :hook
  (minibuffer-setup . highlight-parentheses-minibuffer-setup))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package move-dup
  :ensure t
  :bind
  ("M-<down>" . move-dup-move-lines-down)
  ("M-n" . move-dup-move-lines-down)
  ("M-<up>" . move-dup-move-lines-up)
  ("M-p" . move-dup-move-lines-up))


;; Local packages

(let ((local-packages (expand-file-name "packages" user-emacs-directory)))
  (add-to-list 'load-path local-packages)
  (byte-recompile-directory local-packages 0))

(setopt load-prefer-newer t)
(let ((local-lisp (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path local-lisp)

  ;; Load all elisp files sorted by name. Sub-directories and files starting with underline or dot
  ;; are ignored
  (message "Loading elisp source files from %s" local-lisp)
  (dolist (file (directory-files local-lisp nil "^[^_\\.].*\\.el$"))
    (load (file-name-sans-extension file)))

  ;; Compile all elisp source files (.el)
  (message "Compiling elisp source files from %s" local-lisp)
  (byte-recompile-directory local-lisp 0)

  ;; Delete old elisp compiled files (.elc) that doesn't have a eslisp source file (.el) associated
  (message "Deleting unused elisp compiled files from %s" local-lisp)
  (dolist (file (directory-files local-lisp nil ".*\\.elc$"))
    (unless (file-exists-p (expand-file-name (file-name-with-extension file ".el") local-lisp))
      (message "Deleting %s" file)
      (delete-file (expand-file-name file local-lisp)))))


;; Finalization

(setopt gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
(message "Emacs %s started in %s" emacs-version (emacs-init-time))

;;; init.el ends here
