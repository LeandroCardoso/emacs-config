;;; init.el --- Emacs configuration -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso <leandrocardoso@gmail.com>

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;
;; Initialization ;;
;;;;;;;;;;;;;;;;;;;;

(setopt custom-file (expand-file-name "custom-variables.el" user-emacs-directory))
(setopt gc-cons-threshold (* 32 1024 1024))

;; We must require the 'use-package' at the beginning, so the `use-package-verbose' works properly
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;
;; Emacs packages ;;
;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (setopt copy-region-blink-delay 0.25)
  (setopt delete-pair-blink-delay 0.25)
  (setopt frame-resize-pixelwise t)
  (setopt highlight-nonselected-windows t)
  (setopt inhibit-startup-screen t)
  (setopt initial-scratch-message nil)
  (setopt kill-do-not-save-duplicates t)
  (setopt kill-whole-line t)
  (setopt normal-erase-is-backspace nil)
  (setopt ring-bell-function 'ignore)
  (setopt sentence-end-double-space nil)
  (setopt tab-always-indent 'complete)
  (setopt undo-limit (* 1 1024 1024))
  (setopt undo-strong-limit (truncate (* undo-limit 1.5)))
  (setopt use-short-answers t)
  (setopt user-full-name "Leandro Cardoso")
  (setopt user-mail-address "leandrocardoso@gmail.com")
  (setq-default abbrev-mode t) ; enable abbrev-mode by default
  (setq-default fill-column 100)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  (defun major-mode-set-auto-mode ()
    "Set major mode appropriate for new buffers not visiting a file."
    (if buffer-file-name
        (fundamental-mode)
      (let ((buffer-file-name (buffer-name)))
        (set-auto-mode))))

  (setq-default major-mode 'major-mode-set-auto-mode)

  (when (eq system-type 'windows-nt)
    (load (expand-file-name "lisp/config-windows" user-emacs-directory)))

  :hook
  ((text-mode prog-mode conf-mode) . infer-indentation-style)
  (after-save . executable-make-buffer-file-executable-if-script-p)

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
    "Setup `c-mode'.  Provided for use in hooks."
    (c-toggle-comment-style -1))

  (defun c++-setup ()
    "Setup `c++-mode'.  Provided for use in hooks.")

  (defun c-c++-setup ()
    "Setup `c-mode' and `c++-mode'.  Provided for use in hooks."
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

(use-package isearch
  :defer t
  :config
  (setopt isearch-allow-motion t)
  (setopt isearch-allow-scroll 'unlimited)
  (setopt isearch-lazy-count t)
  (setopt isearch-repeat-on-direction-change t)
  (setopt lazy-highlight-initial-delay 0)

  :bind
  (:map isearch-mode-map
        ("<escape>" . isearch-abort)
        ("M-y" . isearch-yank-pop)))

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

(use-package newcomment
  :config
  (setq-default comment-column 0)

  (defun comment-column-setup ()
    "Setup `comment-column' to the default value."
    (setq comment-column (default-value 'comment-column)))

  :hook
  ;; Some modes (like emacs-lisp-mode) have the bad habit of overwriting comment-column. This
  ;; workaround this behavior.
  (prog-mode . comment-column-setup))

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
  (:map ctl-x-map
        ("C-p" . list-packages)))

(use-package replace ; occur
  :defer t
  :config
  (defun truncate-lines-on ()
    "Enable `truncate-lines'.  Provided for use in hooks.

This function disable the `truncate-lines' when `visual-line-mode' is
turned on, as it could produce confusing results."
    (setopt truncate-lines (not visual-line-mode)))

  :hook
  (occur-mode . truncate-lines-on)

  :bind
  (:map occur-mode-map
        ("<tab>" . occur-next)
        ("<backtab>" . occur-prev)
        ("k" . keep-lines)
        ("f" . flush-lines)))

(use-package saveplace
  :config
  (save-place-mode))

(use-package shortdoc
  :defer t
  :config
  (add-hook 'help-fns-describe-function-functions 'shortdoc-help-fns-examples-function))

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

(use-package server
  :config
  (unless noninteractive
    (if (server-running-p)
        (message "Server already running")
      (message "Starting server")
      (server-start))))

(use-package so-long
  :config
  (global-so-long-mode))

(use-package sql
  :defer t
  :config
  (setopt sql-input-ring-file-name  (expand-file-name "sql-history" user-emacs-directory)))

(use-package subword
  :config
  (global-subword-mode))

(use-package use-package
  :config
  (setopt use-package-enable-imenu-support t)
  (setopt use-package-verbose t))

(use-package tooltip
  :config
  (setopt tooltip-resize-echo-area t)
  (tooltip-mode -1))

(use-package tramp
  :defer t
  :config
  (setopt tramp-verbose 2))

(use-package uniquify
  :config
  (setopt uniquify-buffer-name-style 'post-forward))

(use-package vc
  :defer t
  :config
  (setopt diff-font-lock-prettify t)
  (setopt vc-command-messages t)
  (setopt vc-find-revision-no-save t)
  (setopt vc-follow-symlinks t)

  ;; git
  (setopt vc-git-print-log-follow t)

  :bind
  (:map vc-prefix-map
        ("C-d" . vc-dir-root)
        ("e" . 'vc-ediff)
        ("R" . 'vc-rename-file)))

(use-package which-key
  :demand t
  :config
  (setopt which-key-max-description-length 64)
  (setopt which-key-sort-order 'which-key-local-then-key-order)
  (setopt which-key-idle-secondary-delay 0.0)
  (which-key-mode)

  :bind
  (:map help-map
        ("M-x" . which-key-show-top-level)
        ("M-X" . which-key-show-major-mode)))

(use-package windmove
  :config
  ;; See `framemove' for frame related functionality

  ;; Enable windmove - CTRL was chosen because it is the only modifier not used by org-mode
  (windmove-default-keybindings 'ctrl)
  (windmove-swap-states-default-keybindings '(ctrl shift)))

(use-package winner
  :demand t
  :config
  (winner-mode)

  :bind
  (:map winner-mode-map
        ("C-c [" . winner-undo)
        ("C-c ]" . winner-redo)))

(use-package woman
  :init
  ;; unset compose-mail keys to use it with woman
  (global-unset-key (kbd "C-x m"))   ; compose-mail
  (global-unset-key (kbd "C-x 4 m")) ; compose-mail-other-window
  (global-unset-key (kbd "C-x 5 m")) ; compose-mail-other-frame

  :defer t
  :config
  (setopt woman-fill-frame t)

  :bind
  (:map ctl-x-map
        ("m" . woman)))


;;;;;;;;;;;;;;;;;;;;;;;
;; External packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode)

  :hook
  (minibuffer-setup . highlight-parentheses-minibuffer-setup))

(use-package isearch-dabbrev
  :ensure t
  :defer t
  :bind
  (:map isearch-mode-map
        ("<tab>" . isearch-dabbrev-expand)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package move-dup
  :ensure t
  :bind
  ("M-<down>" . move-dup-move-lines-down)
  ("M-n" . move-dup-move-lines-down)
  ("M-<up>" . move-dup-move-lines-up)
  ("M-p" . move-dup-move-lines-up))

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package symbol-overlay
  :ensure t
  :defer t
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package transpose-frame
  :ensure t
  :defer t
  :bind
  (:map ctl-x-map
        ("|" . rotate-frame-clockwise)
        ("\\" . rotate-frame)))

(use-package treesit-auto
  :ensure t
  :config
  (setopt treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode))

(use-package ws-butler
  :ensure t
  :config
  (setopt ws-butler-keep-whitespace-before-point nil)

  (defun ws-butler-mode-on ()
    "Enable `ws-butler-mode'.  Provided for use in hooks."
    (when (and ws-butler-global-mode
               (not (derived-mode-p ws-butler-global-exempt-modes)))
      (ws-butler-mode 1)))

  (defun ws-butler-mode-off ()
    "Disable `ws-butler-mode'.  Provided for use in hooks."
    (ws-butler-mode -1))

  (ws-butler-global-mode)

  :hook
  ;; Disable `ws-butler' when a buffer is being edited by ediff and re-enable when quitting ediff
  (ediff-cleanup . ws-butler-mode-on)
  (ediff-startup . ws-butler-mode-off))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (setopt yas-wrap-around-region t)
  (yas-reload-all)

  :hook
  ((prog-mode text-mode) . yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;
;; Local packages ;;
;;;;;;;;;;;;;;;;;;;;

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

(use-package framemove
  :config
  ;; This requires `windmove'
  (setq framemove-hook-into-windmove t))

(use-package goto-chg
  :bind
  ("C-." . goto-last-change)
  ("C-," . goto-last-change-reverse))


;;;;;;;;;;;;;;;;;;
;; Finalization ;;
;;;;;;;;;;;;;;;;;;

(setopt gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
(message "Emacs %s started in %s" emacs-version (emacs-init-time))

;;; init.el ends here
