;;; init.el --- Emacs configuration -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso <leandrocardoso@gmail.com>

;;; Commentary:

;;; Code:

;; Initialization

(setopt custom-file (concat user-emacs-directory "custom-variables.el"))
(setopt gc-cons-threshold (* 32 1024 1024))


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

(use-package help
  :defer t
  :config
  (setopt describe-bindings-outline t)
  (setopt help-enable-symbol-autoload t))

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
  :defer t
  :config
  (global-so-long-mode))

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
