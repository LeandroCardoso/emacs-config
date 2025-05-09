;;; init.el --- Emacs configuration -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso <leandrocardoso@gmail.com>

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;
;; Initialization ;;
;;;;;;;;;;;;;;;;;;;;

(setopt custom-file (expand-file-name "custom-variables.el" user-emacs-directory))
(setopt gc-cons-threshold (* 32 1024 1024)) ; Increase GC threshold for performance

;; We must require the 'use-package' at the beginning, so the `use-package-compute-statistics' and
;; `use-package-verbose' works properly
(require 'use-package)

(setopt load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package solarized
  :ensure solarized-theme
  :demand t
  :config
  (setopt solarized-distinct-doc-face t)
  (setopt solarized-scale-outline-headlines nil)
  (setopt solarized-scale-org-headlines nil)
  (setopt solarized-use-more-italic t)
  (setopt solarized-use-variable-pitch nil)

  (load-theme 'solarized-dark t)

  (require 'solarized-palettes)

  (setq solarized-custom-faces
        '("My personal solarized theme customizations"
          (custom-theme-set-faces
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
           `(symbol-overlay-default-face ((t :inherit unspecified :foreground ,magenta))))
          (custom-theme-set-variables
           theme-name
           `(ibuffer-filter-group-name-face 'link)
           `(ibuffer-title-face 'header-line))))

  (solarized-with-color-variables
    'dark 'solarized-dark solarized-dark-color-palette-alist solarized-custom-faces))

;; TODO Set font and font size

(make-frame-visible)

;; TODO move this function to another file
;; TODO clean up subdirectories
(defun byte-recompile-and-cleanup-directory (directory &optional force follow-symlinks)
  "Recompile and clean eslip files in DIRECTORY.

Recompile every ‘.el’ file in DIRECTORY that needs recompilation.  This
happens when a '.elc' file doesn't exist, or it exists but is older than
the '.el' file.  Files in subdirectories of DIRECTORY are processed
also.

After recompilation, delete old stale '.elc' files that don't have a
corresponding '.el' associated file.

If the argument FORCE is non-nil, recompile every '.el'.

This command will normally not follow symlinks when compiling files.  If
FOLLOW-SYMLINKS is non-nil, symlinked '.el' files will also be compiled."
  (require 'bytecomp)
  ;; Compile all elisp files
  (message "Compiling elisp files in %s..." directory)
  (byte-recompile-directory directory 0 force follow-symlinks)

  ;; Delete old elisp compiled files (.elc) that doesn't have a eslisp source file (.el) associated
  (message "Cleaning stale elisp compiled files in %s..." directory)
  (let ((deleted 0)
        (skipped 0))
    (dolist (f (directory-files directory t ".*\\.elc$"))
      (let ((file (expand-file-name f directory)))
        (unless (file-exists-p (file-name-with-extension file ".el"))
          (message "Deleting file: %s" file)
          (delete-file file))
        (if (file-exists-p file)
            (setq skipped (1+ skipped))
          (setq deleted (1+ deleted)))))
    (message "Done (Total of %d files deleted, %d skipped)" deleted skipped)))

(byte-recompile-and-cleanup-directory (expand-file-name "packages" user-emacs-directory))
(byte-recompile-and-cleanup-directory (expand-file-name "lisp" user-emacs-directory))


;;;;;;;;;;;;;;;;;;;;
;; Emacs packages ;;
;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (setopt frame-inhibit-implied-resize t) ;; never resize the frame
  (setopt frame-resize-pixelwise t)
  (setopt highlight-nonselected-windows t)
  (setopt inhibit-startup-screen t)
  (setopt initial-scratch-message nil)
  (setopt ring-bell-function 'ignore)
  (setopt use-short-answers t)
  (setopt x-underline-at-descent-line t)
  (setq inhibit-compacting-font-caches t)
  (setq-default cursor-type 'bar)

  ;; Edit
  (setopt delete-pair-blink-delay 0.25)
  (setopt sentence-end-double-space nil)
  (setopt tab-always-indent 'complete)
  (setq-default abbrev-mode t) ; enable abbrev-mode by default
  (setq-default fill-column 100)
  (setq-default tab-width 4)

  ;; Fringe
  (setq-default indicate-empty-lines t)
  (setopt next-error-highlight 'fringe-arrow)

  ;; Modeline
  (setopt column-number-mode t)
  (setopt mode-line-default-help-echo nil)
  (setopt mode-line-position-column-line-format '(" %l:%c"))

  ;; Undo
  (setopt undo-limit (* 1 1024 1024))
  (setopt undo-strong-limit (truncate (* undo-limit 1.5)))

  ;; User
  (setopt user-full-name "Leandro Cardoso")
  (setopt user-mail-address "leandrocardoso@gmail.com")

  ;; Platform specific settings
  (when (eq system-type 'windows-nt)
    (load (expand-file-name "lisp/config-mswindows" user-emacs-directory)))

  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)

  :bind
  ("<escape>" . execute-extended-command)
  ("C-<backspace>" . backward-kill-sexp)
  ("C-c D" . delete-pair)
  ("C-c d" . duplicate-dwim)
  ([remap zap-to-char] . zap-up-to-char))

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

(use-package face-remap
  :defer t
  :config
  (setopt text-scale-mode-step 1.1))

(use-package find-file
  :defer t
  :config
  (setopt cc-search-directories '("." "./*" "../*" "/usr/include" "/usr/local/include/*")))

(use-package goto-addr
  :config
  (global-goto-address-mode))

(use-package grep
  :defer t
  :config
  (setopt grep-save-buffers nil)
  (setopt grep-use-headings t)

  (let ((cc "*.cc *.cxx *.cpp *.[Cc] *.CC *.c++")
        (hh "*.hxx *.hpp *.[Hh] *.HH *.h++")
        (cext "*.def *.rc"))
    (setq grep-files-aliases (assoc-delete-all "cc" grep-files-aliases))
    (setq grep-files-aliases (assoc-delete-all "cchh" grep-files-aliases))
    (setq grep-files-aliases (assoc-delete-all "cx" grep-files-aliases))
    (push `("cc" . ,cc) grep-files-aliases)
    (push `("cchh" . ,(concat cc " " hh)) grep-files-aliases)
    (push `("cx" . ,(concat cc " " hh " " cext)) grep-files-aliases))

  (add-to-list 'grep-files-aliases '("cs" . "*.cs"))
  (add-to-list 'grep-files-aliases '("web" . "*.css *.htm[l] *.js *.json *.ts"))

  (dolist (file '("TAGS*" "GPATH" "GRTAGS" "GTAGS"                           ;tags
                  "main.*.js" "polyfills.*.js" "runtime.*.js" "styles.*.css" ;minified
                  "*.cache" "*.exe" "*.nupkg" "*.so" "*.zip"))               ;misc
    (add-to-list 'grep-find-ignored-files file))

  ;; Enable saving the latest regexp into the `kill-ring'
  (advice-add 'lgrep :after 'kill-new-advice)
  (advice-add 'rgrep :after 'kill-new-advice)
  (advice-add 'zrgrep :after 'kill-new-advice)

  :hook
  (grep-setup . truncate-lines-on)

  :bind
  ("C-c g" . rgrep)
  ("C-c G" . zrgrep)
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
    dashboard-mode
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
  (:map org-mode-map
        ("C-c M-t" . org-toggle-link-display)
        ;; workaround to avoid override by a global key
        ("M-<return>" . org-meta-return))
  ("C-c o" . org-out-keymap)
  (:map org-out-keymap
        ("l" . org-store-link)
        ("t" . orgtbl-mode)
        ("s" . orgalist-mode)))

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
  ("C-c <tab>" . indent-tabs-mode)
  ("C-c k" . kill-whole-line)
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

(use-package use-package
  :config
  (setopt use-package-compute-statistics t) ; view the statistical report using `use-package-report'
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
        ("e" . 'vc-ediff)
        ("R" . 'vc-rename-file)))

(use-package which-key
  :demand t
  :config
  (setopt which-key-max-description-length 64)
  (setopt which-key-sort-order 'which-key-local-then-key-order)
  (setopt which-key-idle-secondary-delay 0.0)
  (which-key-mode))

(use-package window
  :config
  (setopt split-height-threshold nil)
  (setopt split-width-threshold 200))

(use-package whitespace
  :defer t
  :init
  (defvar whitespace-keymap (make-sparse-keymap) "Keymap for whitespace commands")
  (defalias 'whitespace-keymap whitespace-keymap)

  :config
  (setopt whitespace-line-column nil) ; use `fill-column' value

  :bind
  (("C-c w" . whitespace-keymap)
   (:map whitespace-keymap
         ("c" . whitespace-cleanup)
         ("n" . whitespace-newline-mode)
         ("o" . whitespace-toggle-options)
         ("r" . delete-whitespace-rectangle) ; rect.el
         ("t" . delete-trailing-whitespace)  ; simple.el
         ("w" . whitespace-mode))))

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

(use-package dashboard
  :ensure t
  :init
  (setopt dashboard-icon-type (if (eq system-type 'windows-nt)
                                  nil
                                'nerd-icons))

  :config
  (setopt dashboard-items '((recents . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5)))
  (setopt dashboard-set-file-icons t)
  (setopt dashboard-set-heading-icons t)
  (setopt dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook)

  :bind
  (:map dashboard-mode-map
        ("C-<tab>" . dashboard-next-section)
        ("C-<iso-lefttab>" . dashboard-previous-section)
        ("C-S-<tab>" . dashboard-previous-section)))

(use-package doom-modeline
  :ensure t
  :demand t
  :config
  (setopt doom-modeline-buffer-file-name-style 'file-name-with-project)
  (setopt doom-modeline-height 30)
  (setopt doom-modeline-indent-info t)
  (setopt doom-modeline-vcs-max-length 22)
  (doom-modeline-mode 1)

  :custom-face
  ;; FIXME
  (doom-modeline-bar ((t (:background ,(face-foreground 'mode-line-buffer-id))))))

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
  (ivy-mode 1))

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

  (defun tide-setup-hook ()
    (tide-setup))

  ;; format the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'tide-setup-hook)

  ;; Force bundled tide tsserver
  (defun tide-force-bundled-tsserver ()
    "Force use of bundled tide tsserver.
See `tide-tsserver-executable'."
    (interactive)
    (setopt tide-tsserver-executable
            (car (directory-files-recursively (concat user-emacs-directory "elpa/") tide--tsserver)))))

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

  :hook
  ((prog-mode text-mode) . yas-minor-mode))


;;;;;;;;;;;;;;;;;;;;
;; Local packages ;;
;;;;;;;;;;;;;;;;;;;;

(let ((local-lisp (expand-file-name "lisp" user-emacs-directory)))
  ;; Load all elisp files sorted by name. Sub-directories and files starting with underline or dot
  ;; are ignored
  (message "Loading elisp source files from %s" local-lisp)
  (dolist (file (directory-files local-lisp nil "^[^_\\.].*\\.el$"))
    (load (file-name-sans-extension file))))

(use-package arc-mode-extra
  :defer t
  :after arc-mode
  :bind
  (:map archive-mode-map
        ([remap back-to-indentation] . archive-move-to-filename)))

(use-package dashboard-desktop
  :after dashboard
  :config
  (add-to-list 'dashboard-items '(desktop . 5))

  :hook
  (dashboard-after-initialize . dashboard-jump-to-desktop))

(use-package default-font-height
  :config
  (default-font-height-setup)

  :bind
  ("C-M-=" . default-font-height-adjust))

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
  ;; This extends ediff
  :defer t
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

  :bind
  ("M-#" . base64-encode-dwim)
  ([remap delete-blank-lines] . delete-all-blank-lines)
  ("C-M-|" . delete-indentation)
  ([remap kill-line] . kill-line-and-join)
  ("C-h" . mark-line) ; original is help prefix, but we have f1 for it
  ("M-<return>" . newline-no-break))

(use-package frame-window-extra
  :defer t
  :config
  (setopt split-window-preferred-function 'split-window-sensibly-horizontally)

  (setopt preferred-font-list '("Source Code Pro" "Cascadia Mono" "Consolas"))
  (set-preferred-font)

  :bind
  ("M-o" . other-window)
  ("M-O" . other-window-backward)
  ([remap toggle-frame-fullscreen] . toggle-frame-fullscreen+)
  (:map ctl-x-map
        ("o" . other-frame)) ; original is other-window
  (:map ctl-x-4-map
        ("k" . kill-other-buffer-and-window)))

(use-package framemove
  :after windmove
  :config
  (setq framemove-hook-into-windmove t))

(use-package goto-chg
  :bind
  ("C-." . goto-last-change)
  ("C-," . goto-last-change-reverse))

(use-package gtags-mode-extra
  :defer t
  :after gtags-mode
  :bind
  (:map project-prefix-map
        ("t" . gtags-mode-project-create)))

(use-package ibuffer-extra
  :defer t
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

(use-package rotate-text
  :defer t
  :config
  (setopt rotate-text-words
        '(("height" "width")
          ("left" "right" "top" "bottom")
          ("active" "inactive")
          ("enable" "disable")
          ("enabled" "disabled")
          ("true" "false")
          ("yes" "no")))
  (setopt rotate-text-symbols
        '(("private" "protected" "public")
          ("on" "off")
          ("t" "nil")))

  :bind
  ("C-=" . rotate-text)
  ("C-+" . rotate-text-backward))

(use-package w32-extra)


;;;;;;;;;;;;;;;;;;
;; Finalization ;;
;;;;;;;;;;;;;;;;;;

(setopt gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
(message "Emacs %s started in %s" emacs-version (emacs-init-time))

;;; init.el ends here
