;;; init.el --- Emacs configuration -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso <leandrocardoso@gmail.com>

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;
;; Initialization ;;
;;;;;;;;;;;;;;;;;;;;

(setopt custom-file (expand-file-name "custom-variables.el" user-emacs-directory))
(setopt gc-cons-threshold (* 32 1024 1024)) ; Increase GC threshold for performance
(setopt load-prefer-newer t)

(defconst user-lisp-directory (expand-file-name "lisp" user-emacs-directory)
  "Directory where user's Emacs *.el and *.elc Lisp files are installed.")

(add-to-list 'load-path user-lisp-directory)
(add-to-list 'load-path (expand-file-name "packages" user-lisp-directory))

;; We must config use-package at the beginning, so the `use-package-compute-statistics' and
;; `use-package-verbose' works properly
(use-package use-package
  :config
  (setopt use-package-compute-statistics t) ; view the statistical report using `use-package-report'
  (setopt use-package-enable-imenu-support t)
  ;; (setopt use-package-verbose t)
  (setopt use-package-vc-prefer-newest t)
  ;; Workaround for signature error when managing elpa packages
  (setopt package-check-signature (if (eq system-type 'windows-nt)
                                      nil
                                    'allow-unsigned)))

(use-package solarized
  :ensure solarized-theme
  :demand t
  :config
  (setopt solarized-distinct-doc-face t)
  (setopt solarized-scale-org-headlines nil)
  (setopt solarized-scale-outline-headlines nil)
  (setopt solarized-use-more-italic t)
  (setopt solarized-use-variable-pitch nil)

  (require 'solarized-palettes) ; must be before `solarized-dark-color-palette-alist' reference
  (defun set-custom-faces-solarized-dark-theme ()
    "Set custom faces in solarized dark theme"
    (solarized-with-color-variables
      'dark 'solarized-dark solarized-dark-color-palette-alist
      '((custom-theme-set-faces
         theme-name
         `(Info-quoted ((t :foreground ,orange :inherit font-lock-string-face)))
         `(bookmark-face ((t :inherit fringe)))
         `(button ((t :inherit link)))
         `(cursor ((t :background ,yellow)))
         `(custom-button ((t :inherit button)))
         `(diff-error ((t :inherit error)))
         `(dired-header ((t :inherit dired-directory :weight bold)))
         `(fringe ((t :foreground ,s-line)))
         `(header-line ((t :foreground ,yellow :underline ,yellow :weight bold :extend t)))
         `(help-key-binding ((t :box (:line-width -1 :color ,s-line) :weight bold)))
         `(isearch-group-1 ((t :foreground ,base03 :background ,magenta-1fg :weight bold)))
         `(isearch-group-2 ((t :foreground ,base03 :background ,magenta-2fg :weight bold)))
         `(minibuffer-prompt ((t :foreground ,yellow)))
         `(mode-line ((t :background ,blue-2bg)))
         `(mode-line-buffer-id ((t :foreground ,yellow :weight bold)))
         `(mode-line-inactive ((t :background ,base02)))
         `(region ((t :foreground unspecified :background ,blue-2bg :extend t)))
         `(separator-line ((t :height 0.1 :inherit transient-separator)))
         `(shortdoc-heading ((t :inherit info-title-1)))
         `(shortdoc-section ((t :inherit info-title-1 :weight normal)))
         `(symbol-overlay-default-face ((t :inherit unspecified :foreground ,magenta)))
         `(doom-modeline-bar ((t (:background ,yellow))))
         `(tldr-title ((t :inherit info-title-1)))
         `(tldr-introduction ((t :inherit default)))
         `(tldr-description ((t :inherit info-title-2)))
         `(tldr-command-itself ((t :foreground ,blue :slant italic :weight bold)))
         `(tldr-command-argument ((t :foreground ,blue)))
         `(tldr-code-block ((t :foreground ,blue :weight bold))))
        (custom-theme-set-variables
         theme-name
         `(ibuffer-filter-group-name-face 'link)
         `(ibuffer-title-face 'header-line)))))

  (load-theme 'solarized-dark t)
  (set-custom-faces-solarized-dark-theme))

(make-frame-visible)


;;;;;;;;;;;;;;;;;;;;
;; Emacs packages ;;
;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (setopt confirm-kill-emacs 'y-or-n-p)
  (setopt confirm-kill-processes nil)
  (setopt frame-inhibit-implied-resize t) ; never resize the frame
  (setopt frame-resize-pixelwise t)
  (setopt highlight-nonselected-windows t)
  (setopt inhibit-startup-screen t)
  (setopt initial-scratch-message nil)
  (setopt ring-bell-function 'ignore)
  (setopt save-some-buffers-default-predicate 'save-some-buffers-root)
  (setopt truncate-partial-width-windows nil)
  (setopt use-short-answers t)
  (setopt window-combination-resize t)
  (setopt x-underline-at-descent-line t)
  (setq inhibit-compacting-font-caches t)
  (setq-default cursor-type 'bar)
  (setq-default truncate-lines nil)

  ;; auto-save
  (defconst auto-save-dir (expand-file-name "auto-save" user-emacs-directory)
    "Directory to save auto-save files.")
  (setopt auto-save-file-name-transforms `((".*" ,(concat auto-save-dir "/\\1") t)))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir))

  ;; backup
  (setopt backup-by-copying t)
  (setopt delete-old-versions t)
  (setopt make-backup-files nil)
  (setopt version-control t)

  ;; edit
  (setopt delete-pair-blink-delay 0.25)
  (setopt sentence-end-double-space nil)
  (setopt tab-always-indent 'complete)
  (setq-default abbrev-mode t) ; enable abbrev-mode by default
  (setq-default fill-column 100)
  (setq-default tab-width 4)

  ;; fringe
  (setq-default indicate-empty-lines t)
  (setopt next-error-highlight 'fringe-arrow)

  ;; modeline
  (setopt column-number-mode t)
  (setopt mode-line-default-help-echo nil)
  (setopt mode-line-position-column-line-format '(" %l:%c"))

  ;; scroll
  (setopt scroll-conservatively 10) ; scroll up to this many lines without recentering
  (setopt scroll-preserve-screen-position t)

  ;; trusted lisp files
  (add-to-list 'trusted-content (file-name-as-directory user-lisp-directory))

  ;; undo
  (setopt undo-limit (* 1 1024 1024))
  (setopt undo-strong-limit (truncate (* undo-limit 1.5)))

  ;; user
  (setopt user-full-name "Leandro Cardoso")
  (setopt user-mail-address "leandrocardoso@gmail.com")

  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)

  :bind
  ("<escape>" . execute-extended-command)
  ("C-<backspace>" . backward-kill-sexp)
  ("C-c D" . delete-pair)
  ("C-c d" . duplicate-dwim)
  ([remap zap-to-char] . zap-up-to-char)
  (:map ctl-x-x-map
        ("G" . revert-buffer-with-fine-grain)))

(use-package align
  :defer t
  :bind
  ("C-c a" . align-regexp))

(use-package autorevert
  :config
  (setopt auto-revert-verbose nil)
  (global-auto-revert-mode))

(use-package arc-mode
  :defer t
  :config
  (setopt archive-visit-single-files t)

  :bind
  (:map archive-mode-map
        ("<tab>" . archive-next-line)
        ("<backtab>" . archive-previous-line)))

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

(use-package comint
  :defer t
  :config
  (setopt comint-completion-autolist t)
  (setopt comint-input-ignoredups t)
  (setopt comint-prompt-read-only t))

(use-package compile
  :defer t
  :config
  (setopt compilation-scroll-output 'first-error)
  (setopt compilation-error-screen-columns nil))

(use-package desktop
  :defer t
  :config
  (setopt desktop-save 'ask-if-exists)
  (add-to-list 'desktop-locals-to-save 'buffer-display-time)

  :hook
  (desktop-after-read . clean-buffer-list)
  ;; I am enabling the desktop-save-mode *after* a desktop session is loaded by the `desktop-read'
  ;; command to avoid loading a desktop session on Emacs initialization
  (desktop-after-read . desktop-save-mode))

(use-package dired
  :defer t
  :config
  (setopt dired-auto-revert-buffer t)
  (setopt dired-create-destination-dirs 'ask)
  (setopt dired-dwim-target t)
  (setopt dired-isearch-filenames 'dwim)
  (setopt dired-kill-when-opening-new-dired-buffer t)
  (setopt dired-maybe-use-globstar t)

  :bind
  (:map dired-mode-map
        ("<tab>" . dired-next-line)
        ("<backtab>" . dired-previous-line)
        ("M-<return>" . dired-up-directory)
        ("C-=" . dired-compare-directories)
        ("C-+" . dired-create-empty-file))
  (:map ctl-x-map
        ("M-d" . find-name-dired)))

(use-package ediff
  :defer t
  :config
  (setopt ediff-show-ancestor nil)
  (setopt ediff-split-window-function 'split-window-horizontally)
  ;; everything in one frame)
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ediff-mult
  :defer t
  :config
  (defun ediff-meta-buffer-map-setup ()
    "Setup \"<tab>\" and \"<backtab\" keys. Provided for use in hooks."
    (define-key ediff-meta-buffer-map (kbd "<tab>") 'ediff-next-meta-item)
    (define-key ediff-meta-buffer-map (kbd "<backtab>") 'ediff-previous-meta-item))

  :hook
  (ediff-meta-buffer-keymap-setup . ediff-meta-buffer-map-setup))

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
  :after mode-local
  :config
  (setq-mode-local emacs-lisp-mode sentence-end-double-space t)

  :bind
  (:map emacs-lisp-mode-map
        ;; eval-defun is also in C-M-x)
        ("C-c C-c" . eval-defun)))

(use-package eshell
  :defer t
  :after misc-extra
  :config
  (define-other-window-command eshell)

  :bind
  (:map ctl-x-map
        ("C-$" . eshell))
  (:map ctl-x-4-map
        ("C-$" . eshell-other-window)))

(use-package em-hist ; eshell history
  :defer t
  :after eshell
  :config
  (setopt eshell-hist-ignoredups t))

(use-package em-term ; eshell visual commands
  :defer t
  :after eshell
  :config
  (add-to-list 'eshell-visual-commands "watch"))

(use-package face-remap
  :defer t
  :config
  (setopt text-scale-mode-step 1.1))

(use-package find-file
  :defer t
  :config
  (setopt cc-search-directories '("." "./*" "../*" "/usr/include" "/usr/local/include/*"))
  (setopt ff-case-fold-search t)

  :bind
  ("C-M-o" . ff-find-other-file))

(use-package ffap
  :demand t
  :config
  (setopt ffap-file-name-with-spaces t)
  (ffap-bindings))

(use-package goto-addr
  :config
  (global-goto-address-mode))

(use-package grep
  :defer t
  :config
  (setopt grep-save-buffers nil)
  (setopt grep-use-headings t)

  ;; These aliases are also used by xref
  (let ((cc (string-replace "*.C" "*.[Cc]" (alist-get "cc" grep-files-aliases nil nil 'equal)))
        (hh (alist-get "hh" grep-files-aliases nil nil 'equal))
        (extra "*.def *.rc"))
    ;; Original cc and cchh lacks *.c
    (setf (alist-get "cc" grep-files-aliases nil nil 'equal) cc)
    (setf (alist-get "cchh" grep-files-aliases nil nil 'equal) (concat cc " " hh))
    (add-to-list 'grep-files-aliases `("cx" . ,(concat cc " " hh " " extra))))

  (add-to-list 'grep-files-aliases '("cs" . "*.cs"))
  (add-to-list 'grep-files-aliases '("web" . "*.css *.htm[l] *.js *.json *.ts"))

  (dolist (file '("TAGS*" "GPATH" "GRTAGS" "GTAGS"                           ; tags
                  "main.*.js" "polyfills.*.js" "runtime.*.js" "styles.*.css" ; minified
                  "*.cache" "*.exe" "*.nupkg" "*.so" "*.zip"))               ; misc
    (add-to-list 'grep-find-ignored-files file))

  ;; Enable saving the latest regexp into the `kill-ring'
  (advice-add 'lgrep :after 'kill-new-advice)
  (advice-add 'rgrep :after 'kill-new-advice)
  (advice-add 'zrgrep :after 'kill-new-advice)

  :hook
  (grep-setup . truncate-lines-on)

  :bind
  (:map grep-mode-map
        ("u" . rename-uniquely)
        ("k" . keep-lines)
        ("f" . flush-lines)))

(use-package help
  :defer t
  :config
  (setopt describe-bindings-outline t)
  (setopt help-enable-symbol-autoload t)

  :bind
  (:map help-map
        ("B" . describe-personal-keybindings)
        ("M-x" . which-key-show-top-level)
        ("M-X" . which-key-show-major-mode)))

(use-package hi-lock
  :config
  (global-hi-lock-mode))

(use-package hi-line
  :config
  ;; hl-line-mode causes slowness when scrolling down repeatedly, this is a workaround for it
  (setq auto-window-vscroll nil)

  :hook
  ((archive-mode
    dired-mode
    grep-mode
    ibuffer-mode
    occur-mode
    proced-mode
    tabulated-list-mode
    tar-mode) . hl-line-mode))

(use-package ibuffer
  :defer t
  :config
  (setopt ibuffer-display-summary nil)
  ;; increase the name size
  (setopt ibuffer-formats '((mark modified read-only locked
                                  " " (name 48 48 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide)
                                  " " filename-and-process)
                            (mark " " (name 48 48) " " filename)))
  (setopt ibuffer-marked-char ?*)
  (setopt ibuffer-modified-char ?M)
  (setopt ibuffer-read-only-char ?R)


  :bind
  ([remap list-buffers] . ibuffer)
  (:map ctl-x-4-map
        ("C-b" . ibuffer-other-window)))

(use-package imenu
  :defer t
  :config
  (setopt imenu-auto-rescan t)
  (setopt imenu-flatten 'prefix)
  (setopt imenu-max-item-length nil) ; don't truncate Imenu entries

  :bind
  ("C-z" . imenu)) ; original is suspend-frame

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

(use-package ispell
  :defer t
  :config
  (setenv "DICTIONARY" "en_US")
  (setopt ispell-dictionary "en_US")
  (setopt ispell-help-in-bufferp 'electric)
  (setopt ispell-personal-dictionary
          (expand-file-name (concat "dict_" ispell-dictionary) user-emacs-directory))
  (setopt ispell-program-name "hunspell")
  (setopt ispell-query-replace-choices t)
  (setopt ispell-silently-savep t)

  (setopt ispell-complete-word-dict
          (let ((word-dict (expand-file-name "words.txt" user-emacs-directory)))
            (when (file-exists-p word-dict)
              word-dict))))

(use-package midnight
  :config
  (setopt clean-buffer-list-delay-general 2)
  (midnight-mode))

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

(use-package nxml-mode
  :defer t
  :mode "\\.xaml\\'"
  :config
  (setopt nxml-child-indent (default-value 'tab-width))
  (setopt nxml-slash-auto-complete-flag t))

(use-package org
  :defer t
  :init
  (defvar org-out-keymap (make-sparse-keymap) "Keymap for org-mode commands outside org-mode")
  (defalias 'org-out-keymap org-out-keymap)

  :config
  (setopt org-M-RET-may-split-line '((default . nil)))           ; don't split the line at the cursor position when ALT+ENTER
  (setopt org-blank-before-new-entry '((heading . nil)
                                       (plain-list-item . nil))) ; don't automatically put new line chars
  (setopt org-fontify-done-headline nil)                         ; don't change the face of a headline if it is marked DONE
  (setopt org-hierarchical-todo-statistics nil)                  ; all entries in the subtree are considered
  (setopt org-imenu-depth 3)                                     ; maximum level for Imenu access
  (setopt org-level-color-stars-only t)                          ; fontify only the stars in each headline
  (setopt org-outline-path-complete-in-steps nil)                ; display everything
  (setopt org-special-ctrl-a/e t)                                ; special headline handling
  (setopt org-src-window-setup 'current-window)                  ; show edit buffer in the current window
  (setopt org-startup-truncated nil)                             ; don't set `truncate-lines', this break long tables
  (setopt org-tag-faces `(("doubt" . ,(face-foreground 'warning))
                          ("important" . ,(face-foreground 'org-warning))))
  (setopt org-tag-persistent-alist '(("doubt" . ?d)
                                     ("important" . ?i)))        ; tags always available in Org files
  (setopt org-tags-column (- fill-column))                       ; align tags at the right margin
  (setopt org-tags-sort-function 'string<)                       ; align tags using alphabetic order
  (setopt org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "BLOCKED(b)" "|" "DONE(d)" "CANCELED(c)")))
  (setopt org-todo-keyword-faces `(("WAITING" . (:foreground ,(face-foreground 'warning) :weight bold))
                                   ("BLOCKED" . (:foreground ,(face-foreground 'warning) :weight bold))))

  :bind
  ("C-c o" . org-out-keymap)
  (:map org-out-keymap
        ("l" . org-store-link)
        ("t" . orgtbl-mode)
        ("s" . orgalist-mode))
  (:map org-mode-map
        ("C-c M-t" . org-toggle-link-display)
        ;; workaround to avoid override by a global key
        ("M-<return>" . org-meta-return)))

(use-package ox ; org export
  :defer t
  :after org
  :config
  (setopt org-export-copy-to-kill-ring 'if-interactive)
  (setopt org-export-initial-scope 'subtree)
  (setopt org-export-preserve-breaks t)
  (setopt org-export-with-author nil)
  (setopt org-export-with-sub-superscripts nil)
  (setopt org-export-with-title nil)
  (setopt org-export-with-toc nil))

(use-package ox-ascii ; org export ascii
  :defer t
  :after ox
  :config
  (setopt org-ascii-caption-above t)
  (setopt org-ascii-text-width most-positive-fixnum))

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

(use-package prog-mode
  :defer t
  :bind
  (:map prog-mode-map
        ("<f9>" . compile)
        ("C-<tab>" . company-indent-or-complete-common)))

(use-package project
  :defer t
  :config
  (setopt project-vc-merge-submodules nil))

(use-package recentf
  :config
  (setopt recentf-max-saved-items 200)
  (recentf-mode))

(use-package replace ; occur
  :defer t
  :hook
  (occur-mode . truncate-lines-on)

  :bind
  (:map occur-mode-map
        ("<tab>" . occur-next)
        ("<backtab>" . occur-prev)))

(use-package saveplace
  :config
  (save-place-mode))

(use-package shell
  :defer t
  :after misc-extra
  :config
  (define-other-window-command shell)

  :bind
  (:map ctl-x-map
        ("$" . shell))
  (:map ctl-x-4-map
        ("$" . shell-other-window)))

(use-package shortdoc
  :defer t
  :config
  (add-hook 'help-fns-describe-function-functions 'shortdoc-help-fns-examples-function))

(use-package simple
  :config
  (defun truncate-lines-on ()
    "Enable `truncate-lines' in the current buffer.  Provided for use in hooks.

This function enables `truncate-lines', unless `visual-line-mode' is
turned on, as it could produce confusing results."
    (setopt truncate-lines (not visual-line-mode)))

  (defun kill-new-advice (string &rest r)
    "Make STRING the latest kill in the kill ring.

This function is intended to be used as an advice.  Parameter R exists
for compatibility only.

See `kill-new' for details."
    (kill-new string))

  (setopt copy-region-blink-delay 0.25)
  (setopt eval-expression-print-length nil)
  (setopt goto-line-history-local t)
  (setopt kill-do-not-save-duplicates t)
  (setopt kill-whole-line t)
  (setopt next-error-message-highlight t)
  (setopt normal-erase-is-backspace nil)
  (setopt shift-select-mode nil)
  (setopt what-cursor-show-names t)
  (setq-default indent-tabs-mode nil)

  :bind
  ("C-M-|" . delete-indentation)
  ("C-c <tab>" . indent-tabs-mode)
  ([remap kill-buffer] . kill-current-buffer)
  ;; special mode like occur and xref
  (:map special-mode-map
        ("f" . flush-lines)
        ("k" . keep-lines)
        ("u" . rename-uniquely))
  ;; capitalize keys
  ([remap capitalize-word] . capitalize-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap upcase-word] . upcase-dwim))

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

(use-package sort
  :defer t
  :bind
  ("C-c s" . sort-lines))

(use-package sql
  :defer t
  :config
  (setopt sql-input-ring-file-name  (expand-file-name "sql-history" user-emacs-directory)))

(use-package subword
  :config
  (global-subword-mode))

(use-package text-mode
  :defer t
  :bind
  (:map text-mode-map
        ("C-<tab>" . company-indent-or-complete-common)))

(use-package time
  :defer t
  :config
  (setopt display-time-24hr-format t)
  (setopt display-time-interval 15)
  (setopt display-time-load-average-threshold 0.30))

(use-package tooltip
  :config
  (setopt tooltip-resize-echo-area t)
  (tooltip-mode -1))

(use-package tramp
  :defer t
  :config
  (setopt tramp-verbose 2))

(use-package transient
  :defer t
  :config
  (setopt transient-default-level 7))

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
        ("e" . vc-ediff)
        ("R" . vc-rename-file)))

(use-package which-key
  :demand t
  :config
  (setopt which-key-max-description-length 64)
  (setopt which-key-sort-order 'which-key-local-then-key-order)
  (setopt which-key-idle-secondary-delay 0.0)
  (which-key-mode))

(use-package window
  :config
  (setopt display-buffer-alist '(("\\`\\*\\(Warnings\\|Compile-Log\\|Backtrace\\)\\*\\'"
                                  (display-buffer-reuse-window display-buffer-use-some-window))))
  (setopt split-height-threshold 80)
  (setopt split-width-threshold 200)

  :bind
  ("S-<up>" . scroll-down-line)
  ("C-S-p" . scroll-down-line)
  ("S-<down>" . scroll-up-line)
  ("C-S-n" . scroll-up-line)
  (:map ctl-x-map
        ("K" . kill-buffer-and-window)))

(use-package whitespace
  :defer t
  :init
  (defvar whitespace-keymap (make-sparse-keymap) "Keymap for whitespace commands")
  (defalias 'whitespace-keymap whitespace-keymap)

  :config
  (setopt whitespace-line-column nil) ; use `fill-column' value

  :bind
  ("C-c w" . whitespace-keymap)
  (:map whitespace-keymap
        ("c" . whitespace-cleanup)
        ("n" . whitespace-newline-mode)
        ("o" . whitespace-toggle-options)
        ("r" . delete-whitespace-rectangle) ; rect.el
        ("t" . delete-trailing-whitespace)  ; simple.el
        ("w" . whitespace-mode)))

(use-package windmove
  ;; See `framemove' for frame related functionality
  :config
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
  :defer t
  :init
  ;; unset compose-mail keys to use it with woman
  (global-unset-key (kbd "C-x m"))   ; compose-mail
  (global-unset-key (kbd "C-x 4 m")) ; compose-mail-other-window
  (global-unset-key (kbd "C-x 5 m")) ; compose-mail-other-frame

  :config
  (setopt woman-fill-frame t)

  :bind
  (:map ctl-x-map
        ("m" . woman)))

(use-package xref
  :defer t
  :after w32-extra
  :config
  (setopt xref-search-program (if (executable-find "rg") 'ripgrep 'grep))
  (setopt xref-show-definitions-function 'xref-show-definitions-completing-read)

  ;; Use bash shell when calling grep/ripgrep, because it fixes the annoying "^M" that can be
  ;; displayed at end of lines.
  (when (eq system-type 'windows-nt)
    (advice-add 'xref-matches-in-files :around 'with-bash-shell)
    (advice-add 'xref-matches-in-directory :around 'with-bash-shell))

  ;; Enable saving the latest regexp into the `kill-ring'
  (advice-add 'xref--find-xrefs :after 'kill-new-advice)
  (advice-add 'xref-matches-in-files :after 'kill-new-advice)

  :hook
  (xref--xref-buffer-mode . truncate-lines-on)

  :bind
  (:map xref--xref-buffer-mode-map
        ("<tab>" . xref-next-line)
        ("<backtab>" . xref-prev-line)
        ("M-<return>" . xref-quit-and-goto-xref)))


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

(use-package bash-completion
  :ensure t
  :defer t
  :after shell
  :config
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete))

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

(use-package crux
  :ensure t
  :defer t
  :bind
  ;; edit
  ("C-c C-q" . crux-indent-defun)
  ([remap kill-line] . crux-kill-and-join-forward)
  ("C-S-<backspace>" . crux-kill-line-backwards)
  ("C-c k" . crux-kill-whole-line)
  ("M-<return>" . crux-smart-open-line)
  ("M-S-<return>" . crux-smart-open-line-above)
  ;; misc
  ("C-c C-S-k" . crux-indent-rigidly-and-copy-to-clipboard)
  ([remap keyboard-quit] . crux-keyboard-quit-dwim)
  ([remap rename-buffer] . crux-rename-buffer-and-file)
  (:map ctl-x-map
        ("I" . crux-find-user-init-file))
  (:map ctl-x-x-map
        ("D" . crux-delete-file-and-buffer)
        ("s" . crux-sudo-edit)))

(use-package doom-modeline
  :ensure t
  :demand t
  :config
  (setopt doom-modeline-buffer-file-name-style 'file-name-with-project)
  (setopt doom-modeline-height 30)
  (setopt doom-modeline-indent-info t)
  (setopt doom-modeline-vcs-max-length 22)
  (doom-modeline-mode))

(use-package edit-server
  :ensure t
  :config
  (setopt edit-server-default-major-mode 'org-mode)
  (setopt edit-server-new-frame-alist
          '((name . "Edit Server")
            (width . 0.5)
            (height . 0.5)
            (fullscreen . nil)))
  (edit-server-start))

(use-package engine-mode
  :ensure t
  :config
  (engine-mode)
  (load (expand-file-name "engine-mode-config" user-emacs-directory)))

(use-package enlight
  :ensure t
  :demand t
  :config
  (setopt initial-buffer-choice #'enlight)
  (setopt enlight-center-horizontally t)
  (setopt enlight-center-vertically t)

  (defun update-enlight-content-advice ()
    "Upate `enlight-content'."
    (let* ((uptime (time-convert (time-since before-init-time) 'integer))
           (subtitle (if (< uptime 300)
                         (format "started in %s\n" (emacs-init-time "%.2f seconds"))
                       (format "%s up\n" (emacs-uptime "%D, %z%h:%.2m")))))
      (setopt enlight-content
              (concat
               (nerd-icons-sucicon "nf-custom-emacs"
                                   :height 2.0 :v-adjust 0
                                   :face '(:inherit nerd-icons-purple))
               (propertize (format " Welcome to Emacs %s\n" emacs-version)
                           'face '(:inherit font-lock-type-face :weight bold))
               (propertize subtitle 'face '(:slant italic))
               (enlight-menu
                '(("\nAction"
                   ("Desktop Read" (desktop-read) "d")
                   ("Edit File" find-file "f")
                   ("Edit Recent File" (recentf-find-file) "r")
                   ("Select Project" project-switch-project "p")
                   ("eshell" (eshell) "e")
                   ("Quit Emacs" (save-buffers-kill-terminal) "Q"))
                  ("\nEmacs User Directory"
                   ("Edit init file" (find-file user-init-file) "i")
                   ("Dired" (dired user-emacs-directory) "D")
                   ("Magit" (magit-status user-emacs-directory) "m"))))))))

  (advice-add 'enlight :before 'update-enlight-content-advice)

  :bind
  ("<f12>" . enlight-open))

(use-package framemove
  :ensure t
  :vc (:url "https://github.com/emacsmirror/framemove.git")
  :after windmove
  :config
  (setq framemove-hook-into-windmove t))

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

(use-package git-link
  :ensure t
  :defer t
  :init
  (defvar git-link-keymap (make-sparse-keymap) "Keymap for git-link commands")
  (defalias 'git-link-keymap git-link-keymap)

  :config
  (setopt git-link-use-commit t)

  :bind
  ("C-c l" . git-link-keymap)
  (:map git-link-keymap
        ("l" . git-link)
        ("c" . git-link-commit)
        ("h" . git-link-homepage)))

(use-package goto-chg
  :ensure t
  :defer t
  :bind
  ("C-." . goto-last-change)
  ("C-," . goto-last-change-reverse))

(use-package gtags-mode
  :ensure t
  :after w32-extra
  :config
  ;; Force .h files to be treated as a C++
  (setenv "GTAGSFORCECPP" "1")

  (gtags-mode)

  ;; Use bash shell when calling global, because it fixes the annoying "^M" that can be displayed at
  ;; end of lines.
  (when (eq system-type 'windows-nt)
    (advice-add 'gtags-mode--exec-sync :around 'with-bash-shell)
    (advice-add 'gtags-mode--exec-async :around 'with-bash-shell))

  :bind
  (:map project-prefix-map
        ("T" . gtags-mode-update)))

(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode)

  :hook
  (minibuffer-setup . highlight-parentheses-minibuffer-setup))

(use-package imenu-anywhere
  :ensure t
  :defer t
  :after imenu
  :config
  (add-to-list 'imenu-anywhere-friendly-modes '(c-mode c++-mode)))

(use-package isearch-dabbrev
  :ensure t
  :defer t
  :bind
  (:map isearch-mode-map
        ("<tab>" . isearch-dabbrev-expand)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode))

;; TODO review js2 and typescript modes
(use-package js2-mode
  :disabled t
  :ensure t
  :defer t
  :config
  ;; Use js2-minor-mode for syntax highlight, jump-to-definition and imenu
  ;; (defun js2-minor-mode-setup ()
  ;;   (setq-local imenu-create-index-function #'js2-mode-create-imenu-index))
  ;; (add-hook 'js2-minor-mode-hook 'js2-minor-mode-setup)
  ;; (define-key js2-minor-mode-map [remap js-find-symbol] #'js2-jump-to-definition)
  (setopt js2-missing-semi-one-line-override t)
  (setopt js2-mode-assume-strict t)

  :hook
  (js-mode . js2-minor-mode))

(use-package magit
  :ensure t
  :defer t
  :after project
  :init
  ;; Snippet from magit-extra package, copied here so I don't need to load magit before I need to
  ;; use it.
  (keymap-set project-prefix-map "m" 'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)

  :config
  (defun magit-diff-extra-stat-arguments-setup ()
    "Setup `magit-diff-extra-stat-arguments'."
    (when-let ((window (get-buffer-window (current-buffer) 'visible)))
      (list (format "--stat-width=%d" (window-width))
            (format "--stat-graph-width=%d" (/ (window-width) 5))
            "--compact-summary")))

  (setopt magit-diff-extra-stat-arguments 'magit-diff-extra-stat-arguments-setup)
  (setopt magit-ediff-dwim-show-on-hunks t)
  (setopt magit-update-other-window-delay 1)

  ;; Windows specific settings
  (when (eq system-type 'windows-nt)
    (setopt magit-process-connection-type nil)
    (setopt magit-refresh-status-buffer nil)

    ;; experimental performance settings
    (setopt magit-diff-highlight-indentation nil)
    (setopt magit-diff-highlight-trailing nil)
    (setopt magit-diff-paint-whitespace nil)
    (setopt magit-diff-highlight-hunk-body nil)
    (setopt magit-diff-refine-hunk nil)
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'server-switch-hook 'magit-commit-diff)) ; remove diff output from commit

  :bind
  (:map magit-mode-map
        ("M-u" . magit-section-up)
        ([remap previous-line] . magit-previous-line)
        ([remap next-line] . magit-next-line))
  (:map magit-file-section-map
        ("SPC" . magit-diff-visit-file-other-window))
  (:map magit-hunk-section-map
        ("SPC" . magit-diff-visit-file-other-window)))

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

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :defer t
  :after nerd-icons
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :ensure t
  :defer t
  :after (dired nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :defer t
  :after (ibuffer nerd-icons)
  :config
  (setopt nerd-icons-ibuffer-formats '((mark modified read-only locked
                                             " " (name 48 48 :left :elide)
                                             " " (size-h 6 -1 :right)
                                             " " (icon 2 2)
                                             (mode 16 16 :left :elide)
                                             " " filename-and-process+)
                                       (mark " " (name 48 48) " " filename)))

  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ox-jira ; org export jira
  :ensure t
  :defer t
  :after ox
  :config
  ;; Override to preserve newlines in paragraphs
  (defun ox-jira-paragraph-override (paragraph contents info)
    "Transcode a PARAGRAPH element from Org to JIRA.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
    (replace-regexp-in-string "\n\\([^\']\\)" " \n\\1" contents))

  (advice-add 'ox-jira-paragraph :override 'ox-jira-paragraph-override))

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setopt plantuml-indent-level 4)
  (setopt plantuml-jar-path (let ((plantuml-dir (expand-file-name "plantuml" user-emacs-directory)))
                              (when (file-directory-p plantuml-dir)
                                (car (directory-files plantuml-dir t "plantuml.*jar")))))
  (setopt plantuml-default-exec-mode (if plantuml-jar-path 'jar 'server))

  ;; Workaround for fix plantuml server url encode. See
  ;; https://github.com/skuro/plantuml-mode/pull/172/commits/88ec2b989b9e7c8ccef5d9c3aa2800758c24e7f5
  (defun plantuml-server-encode-url-override (string)
    "Encode the string STRING into a URL suitable for PlantUML server interactions."
    (let* ((coding-system (or buffer-file-coding-system
                              "utf8"))
           (str (encode-coding-string string coding-system))
           (encoded-string (mapconcat (lambda(x)(format "%02X" x)) str ) ))
      (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string)))

  (advice-add 'plantuml-server-encode-url :override 'plantuml-server-encode-url-override))

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package rotate-text
  :ensure t
  :vc (:url "https://github.com/emacsmirror/rotate-text.git")
  :defer t
  :config
  (setopt rotate-text-words
          '(("active" "inactive")
            ("enable" "disable")
            ("enabled" "disabled")
            ("height" "width")
            ("left" "right" "top" "bottom")
            ("january" "february" "march" "april" "may" "june" "july" "august" "september" "october" "november" "december")
            ("monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday")
            ("true" "false")
            ("yes" "no")))
  (setopt rotate-text-symbols
          '(("and" "or")
            ("private" "protected" "public")
            ("on" "off")
            ("t" "nil")))

  :bind
  ("C-=" . rotate-text)
  ("C-+" . rotate-text-backward))

(use-package symbol-overlay
  :ensure t
  :defer t
  :hook
  (prog-mode . symbol-overlay-mode))

;; TODO review js2 and typescript modes
(use-package tide
  :disabled t
  :ensure t
  :defer t
  :config
  (setopt tide-completion-detailed t)
  (setopt tide-imenu-flatten t)
  (setopt tide-server-max-response-length 10240000)

  ;; format the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'tide-setup)

  ;; Force bundled tide tsserver
  (defun tide-force-bundled-tsserver ()
    "Force use of bundled tide tsserver.
See `tide-tsserver-executable'."
    (interactive)
    (setopt tide-tsserver-executable
            (car (directory-files-recursively (concat user-emacs-directory "elpa/") tide--tsserver)))))

(use-package tldr
  :ensure t
  :defer t
  :config
  (setopt tldr-enabled-categories '("common" "linux")))

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

(use-package wgrep
  :ensure t
  :defer t
  :after grep)

(use-package ws-butler
  :ensure t
  :config
  (setopt ws-butler-keep-whitespace-before-point nil)

  (defun ws-butler-mode-on ()
    "Enable `ws-butler-mode'.  Provided for use in hooks."
    (message "ws-butler-global-mode enabled")
    (ws-butler-global-mode 1))

  (defun ws-butler-mode-off ()
    "Disable `ws-butler-mode'.  Provided for use in hooks."
    (message "ws-butler-global-mode disabled")
    (ws-butler-global-mode -1))

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

  :hook
  ((prog-mode text-mode) . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)


;;;;;;;;;;;;;;;;;;;;
;; Local packages ;;
;;;;;;;;;;;;;;;;;;;;

(use-package arc-mode-extra
  :defer t
  :after arc-mode
  :bind
  (:map archive-mode-map
        ([remap back-to-indentation] . archive-move-to-filename)))

(use-package dynamic-font-size
  :demand t
  :config
  (dynamic-font-size-mode)

  :bind
  ("C-M-=" . dynamic-font-size-adjust))

(use-package dired-extra
  :defer t
  :after dired
  :bind
  (:map dired-mode-map
        ("K" . dired-do-backup)
        ("M-=" . dired-do-ediff)
        ([remap back-to-indentation] . dired-position-at-filename))
  (:map wdired-mode-map
        ([remap back-to-indentation] . dired-position-at-filename)))

(use-package ediff-extra
  :demand t
  :config
  (ediff-extra-setup-copy-AB-to-C)
  (ediff-extra-setup-text-scale)
  (ediff-extra-setup-window-configuration)
  (ediff-extra-setup-global-keymap))

(use-package edit-extra
  :config
  (setq-default major-mode 'set-auto-mode+)

  :hook
  ((text-mode prog-mode conf-mode) . infer-indentation-style)
  (prog-mode . font-lock-todo-setup)

  :bind
  ("M-#" . base64-encode-dwim)
  ([remap delete-blank-lines] . delete-all-blank-lines)
  ("C-h" . mark-line) ; original is help prefix, but we have f1 for it
  ("C-c S" . sort-words)
  (:map prog-mode-map
        ("C-;" . smart-semicolon)))

(use-package files-extra
  :config
  (defun recompile-user-lisp-files ()
    "Recompile and clean up eslip files in `user-lisp-directory'.

See `byte-recompile-and-cleanup-directory'."
    (interactive)
    (byte-recompile-and-cleanup-directory user-lisp-directory))

  :hook
  (emacs-startup . recompile-user-lisp-files)

  :bind
  (:map ctl-x-x-map
        ("w" . copy-file-or-buffer-name-as-kill)
        ("W" . copy-file-or-buffer-name-directory-as-kill)
        ("k" . make-backup-buffer))
  (:map emacs-lisp-mode-map
        ("C-c C-u" . recompile-user-lisp-files)))

(use-package fragment
  :defer t)

(use-package frame-window-extra
  :after window
  :demand t
  :config
  (setopt split-window-preferred-function 'split-window-sensibly-horizontally)
  (advice-add 'window-splittable-p :around 'window-split-dynamic-threshold-advice)

  :bind
  ("M-o" . other-window)
  ("M-O" . other-window-backward)
  ([remap toggle-frame-fullscreen] . toggle-frame-fullscreen+)
  (:map ctl-x-map
        ("o" . other-frame) ; original is other-window
        ("M-k" . kill-other-buffer-and-window)
        ("M-q" . delete-other-window))
  (:map ctl-x-4-map
        ("k" . kill-other-buffer-and-window)
        ("q" . delete-other-window)))

(use-package gtags-mode-extra
  :defer t
  :after gtags-mode
  :bind
  (:map project-prefix-map
        ("t" . gtags-mode-project-create)))

(use-package ibuffer-extra
  :after ibuffer
  :config
  (ibuffer-remove-title-underline-setup))

(use-package ibuffer-project
  :defer t
  :after ibuffer)

(use-package imenu-anywhere-extra
  :defer t
  :after imenu-anywhere
  :bind
  ([remap imenu] . imenu-anywhere-dwim))

(use-package misc-extra
  :config
  (set-first-font '("Source Code Pro" "Cascadia Mono" "Consolas"))

  ;; Display a clock when Emacs is in fullscreen
  (smart-display-time-mode)
  (advice-add 'toggle-frame-fullscreen :after 'smart-display-time-mode)

  ;; Don't cleanup the buffer list when Emacs is idle during weekends and holidays
  (advice-add 'clean-buffer-list :before-while 'clean-buffer-list-check-idle-time-advice))

(use-package msvs
  :if (eq system-type 'windows-nt))

(use-package nxml-context
  :defer t
  :after nxml-mode
  :hook
  (nxml . nxml-which-func-setup)

  :bind
  (:map nxml-mode-map
        ("C-c C-c" . xml-context-tree)))

(use-package project-extra
  :demand t
  :config
  :bind
  ("C-M-g" . project-query-regexp)
  (:map project-prefix-map
        ("i" . project-info)
        ("q" . project-query-regexp)))

(use-package project-root-dir
  :defer t
  :after project)

(use-package rdi
  :if (eq system-type 'windows-nt)
  :demand t)

(use-package recentf-extra
  :defer t
  :after recentf
  :bind
  (:map ctl-x-map
        ("C-r" . recentf-find-file))              ; original is find-file-read-only
  (:map ctl-x-4-map
        ("C-r" . recentf-find-file-other-window)) ; original is find-file-read-only-other-window
  (:map ctl-x-5-map
        ("C-r" . recentf-find-file-other-frame))) ; original is find-file-read-only-other-frame

(use-package resize-window
  :defer t
  :bind
  ("C-]" . enlarge-window+)
  ("C-}" . shrink-window+)
  ("C-M-]" . enlarge-window-horizontally+)
  ("C-M-}" . shrink-window-horizontally+))

(use-package teamcity)

(use-package w32-extra
  :if (eq system-type 'windows-nt)
  :config
  ;; Root directories are added in the beginning
  (w32-add-unix-root-dir "c:/msys64/ucrt64")
  (w32-add-unix-root-dir "c:/msys64")

  ;; Add external utilities to PATH and exec-path
  (dolist (path (list "C:/Program Files/Git/cmd/"
                      "C:/Program Files (x86)/Java/latest/jre-1.8/bin"
                      (expand-file-name "omnisharp/" user-emacs-directory)
                      (expand-file-name "windows_bin/" user-emacs-directory)))
    (w32-add-to-path path))

  ;; Required to enter password for git
  (setenv "SSH_ASKPASS" "c:/Program Files/Git/mingw64/bin/git-askpass.exe")

  ;; nodejs
  (when (file-exists-p "c:/Program Files/nodejs/nodevars.bat")
    (setq explicit-cmdproxy.exe-args '("/k \"\"C:\\Program Files\\nodejs\\nodevars.bat\"\"")))

  ;; map AltGr to Alt using AutoHotKey
  (if (executable-find "altgr2alt")
      (progn
        (message "Starting altgr2alt")
        (start-process "altgr2alt" (messages-buffer) "altgr2alt"))
    (message "Error: altgr2alt not found!")))

(use-package xml-format
  :defer t
  :after nxml-mode
  :bind
  (:map nxml-mode-map
        ("C-c C-q" . xml-format)
        ("C-c C-M-x" . xml-remove-declaration)))


;;;;;;;;;;;;;;;;;;
;; Finalization ;;
;;;;;;;;;;;;;;;;;;

(setopt gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
(message "Emacs %s started in %s" emacs-version (emacs-init-time))

;;; init.el ends here
