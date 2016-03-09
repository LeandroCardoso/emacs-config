(add-to-list 'load-path "~/.emacs.d/lisp")

;; No need to waste precious desktop space with useless GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; initialize packages
(package-initialize)
(dolist (mode '(goto-chg
                wgrep
                woman
                yasnippet
                transpose-frame
                company
                smex
                which-key
                diff-hl))
  (require mode nil t))

;; Theme
(setq monokai-use-variable-pitch nil)
(load-theme 'monokai t)
(set-face-attribute 'cursor nil :background (face-foreground 'mode-line-buffer-id))
(set-face-attribute 'fringe nil :foreground "dark slate gray") ;; dim gray is also a good option

(setq initial-frame-alist '((fullscreen . maximized) (vertical-scroll-bars)))
(setq default-frame-alist initial-frame-alist)
(setq window-system-default-frame-alist '((x . ((alpha . 97)))))

;; My functions
(load "functions")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ansi-color-for-comint-mode t)
 '(auto-compression-mode t nil (jka-compr))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(blink-cursor-mode nil)
 '(bs-alternative-configuration "files")
 '(bs-default-configuration "all-intern-last")
 '(c-basic-offset 4)
 '(c-offsets-alist (quote ((substatement-open . 0) (case-label . +))))
 '(cc-search-directories
   (quote
    ("." "include" "*" "../*" "/usr/include" "/usr/local/include/*")))
 '(column-number-mode t)
 '(comment-column 0)
 '(compilation-scroll-output (quote first-error))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(cursor-type (quote bar))
 '(custom-buffer-done-kill t)
 '(custom-raised-buttons nil)
 '(delete-old-versions t)
 '(delete-selection-mode t nil (delsel))
 '(desktop-path (quote ("." "~/.emacs.d/" "~")))
 '(desktop-save (quote ask-if-exists))
 '(desktop-save-mode t nil (desktop))
 '(diff-mode-hook (quote (diff-make-unified)))
 '(ediff-custom-diff-options "-c -w")
 '(ediff-diff-options "--binary -w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-inhibit-predicate (quote electric-pair-conservative-inhibit))
 '(electric-pair-mode t)
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode)))
 '(ff-case-fold-search t)
 '(fill-column 100)
 '(frame-resize-pixelwise t)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t)
 '(global-subword-mode t)
 '(grep-find-template "find -L . <X> -type f <F> -exec grep <C> -nH -e <R> {} +")
 '(hi-lock-mode t t (hi-lock))
 '(highlight-nonselected-windows t)
 '(hippie-expand-try-functions-list
   (quote
    (yas-hippie-try-expand try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-visible try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-line)))
 '(ibuffer-display-summary nil)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 30 30 :left :elide)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 30 -1)
           " " filename))))
 '(ido-create-new-buffer (quote always))
 '(ido-decorations
   (quote
    (" { " " }" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-ubiquitous-mode nil)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(ido-use-virtual-buffers (quote auto))
 '(imenu-auto-rescan t)
 '(imenu-max-items 200)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(ispell-query-replace-choices t)
 '(ispell-silently-savep t)
 '(jit-lock-stealth-time 1)
 '(kept-new-versions 9)
 '(kept-old-versions 0)
 '(kill-ring-max 300)
 '(lisp-interaction-mode-hook (quote (turn-on-eldoc-mode)))
 '(mouse-avoidance-banish-position
   (quote
    ((frame-or-window . frame)
     (side . right)
     (side-pos . 0)
     (top-or-bottom . top)
     (top-or-bottom-pos . 0))))
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(next-error-highlight (quote fringe-arrow))
 '(normal-erase-is-backspace nil)
 '(nxml-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(org-M-RET-may-split-line nil)
 '(org-adapt-indentation nil)
 '(org-completion-use-ido t)
 '(org-ellipsis (quote org-ellipsis))
 '(org-imenu-depth 10)
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(org-startup-truncated nil)
 '(shift-select-mode nil)
 '(size-indication-mode t)
 '(smex-save-file "~/.emacs.d/.smex-items")
 '(solarized-use-more-italic t)
 '(solarized-use-variable-pitch nil)
 '(sql-input-ring-file-name "~/.emacs.d/sql-history")
 '(tab-width 4)
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(undo-limit 8000000)
 '(undo-strong-limit 12000000)
 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(version-control t)
 '(which-function-mode t nil (which-func))
 '(whitespace-line-column nil)
 '(winner-mode t)
 '(woman-fill-frame t)
 '(woman-use-symbol-font t)
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-no-prompt))))


;; Font
(when (eq system-type 'gnu/linux)
  (set-frame-font "Source Code Pro-11" t t)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 110)) ;; hack to work with emacsclient)


;; Start the emacs server needed by the emacsclient
(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)
    (message "Server started")))


;; Semantic
(setq semantic-default-submodes '(global-semantic-highlight-func-mode
                                  global-semantic-idle-local-symbol-highlight-mode
                                  global-semantic-idle-scheduler-mode
                                  global-semantic-idle-summary-mode
                                  global-semanticdb-minor-mode))
(setq semantic-imenu-summary-function 'semantic-format-tag-name)
(setq semantic-idle-work-parse-neighboring-files-flag t)
(setq semantic-idle-work-update-headers-flag t)
(setq semanticdb-project-root-functions
      (list
       #'(lambda (directory) (locate-dominating-file directory ".git"))
       #'(lambda (directory) (locate-dominating-file directory ".tfignore"))
       #'(lambda (directory) (locate-dominating-file directory "view.dat"))
       #'(lambda (directory) (locate-dominating-file directory ".dir-locals.el"))))
(semantic-mode)


;; Spell
(setq ispell-program-name "hunspell")

;; pulse
(setq pulse-command-advice-flag t)

;; auto modes
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.inf\\'" . conf-mode))
(eval-after-load "csharp"
  '(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))

;; Hooks
;; (add-hook 'text-mode-hook 'ispell-minor-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun my-elisp-hook()
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-list))
(add-hook 'emacs-list-mode-hook 'my-elisp-hook)

(eval-after-load "yasnippet"
  '(progn
     (add-hook 'prog-mode-hook 'yas-minor-mode)
     (add-hook 'text-mode-hook 'yas-minor-mode)))

;; (add-hook 'text-mode-hook 'orgstruct-mode)
;; (add-hook 'text-mode-hook 'orgtbl-mode)

;; FIXME reposition should be before the pulse
;;(add-hook 'imenu-after-jump-hook 'reposition-window)

(defadvice find-tag (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window)
  (pulse-line-hook-function))

(defadvice find-tag-other-window (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window)
  (pulse-line-hook-function))

(defadvice find-tag-other-frame (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window)
  (pulse-line-hook-function))

(ad-activate 'find-tag)
(ad-activate 'find-tag-other-window)
(ad-activate 'find-tag-other-frame)

;; split window
(setq split-height-threshold nil)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-horizontally)

(show-paren-mode)

;; yasnippet needs this
(eval-after-load "yasnippet"
  '(yas-reload-all))

;; List variables
(eval-after-load "grep"
  '(progn
     ;; delete the default c/c++ aliases
     (assq-delete-all (car(assoc "ch" grep-files-aliases)) grep-files-aliases)
     (assq-delete-all (car(assoc "c" grep-files-aliases)) grep-files-aliases)
     (assq-delete-all (car(assoc "cc" grep-files-aliases)) grep-files-aliases)
     (assq-delete-all (car(assoc "cchh" grep-files-aliases)) grep-files-aliases)
     (assq-delete-all (car(assoc "hh" grep-files-aliases)) grep-files-aliases)
     (assq-delete-all (car(assoc "h" grep-files-aliases)) grep-files-aliases)
     ;; add my aliases
     (add-to-list 'grep-files-aliases '("h" . "*.h *.hpp *.hxx"))
     (add-to-list 'grep-files-aliases '("c" . "*.c *.cpp *.cxx"))
     (add-to-list 'grep-files-aliases '("ch" . "*.h *.hpp *.hxx *.c *.cpp *.cxx"))))

;; packages
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; enable windmove - CTRL is only modifier not used by org-mode
(windmove-default-keybindings 'ctrl)

;; enable abbrev-mode by default
(setq-default abbrev-mode t)

;; sml - it needs to be loaded after the custom variables
(when (require 'smart-mode-line nil t)
  (setq sml/theme nil)
  (setq sml/col-number "%c")
  (setq sml/name-width 30)
  (setq sml/no-confirm-load-theme t)
  (setq sml/pos-minor-modes-separator "]")
  (setq sml/pre-modes-separator "[")
  (setq sml/shorten-mode-string "")
  (setq sml/show-file-name nil)
  (setq sml/vc-mode-show-backend t)
  (set-face-attribute 'sml/filename nil :inherit '(sml/global mode-line-buffer-id) :weight 'bold)
  (add-to-list 'sml/replacer-regexp-list '(".*" (lambda (str)
                                                  (if (project-name)
                                                      (concat ":" (project-name) ":")
                                                    (match-string 0 str)))) t)
  (smart-mode-line-enable))

;; Auto Complete
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (ac-config-default)
;;      ;;(ac-set-trigger-key "TAB")
;;      (add-to-list 'ac-sources 'ac-source-words-in-all-buffer t)
;;      (setq-default ac-sources ac-sources)
;;      (add-hook 'c-mode-common-hook (lambda() (add-to-list 'ac-sources 'ac-source-semantic)))))

;; company
(eval-after-load "company"
  '(progn
     (add-hook 'c-mode-common-hook
               (lambda ()
                 (make-local-variable 'company-backends)
                 ;;(push '(company-semantic :with company-yasnippet company-keywords) company-backends)))
                 (push '(company-dabbrev-code :with company-yasnippet company-keywords) company-backends)))
     (add-hook 'nxml-mode-hook
               (lambda ()
                 (make-local-variable 'company-backends)
                 (push '(company-nxml company-dabbrev company-yasnippet) company-backends)))
     ))

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
;; (setq company-dabbrev-time-limit 0.05)

(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-code-ignore-case t)
;; (setq company-dabbrev-code-time-limit 0.05)

(setq company-auto-complete t)
(setq company-auto-complete-chars "([{.-")
;; TODO add c++ keywords to company-keywords-alist
;; (alignas alignof char16_t char32_t constexpr decltype noexcept nullptr static_assert thread_local)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 3)
(setq company-show-numbers t)
(setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
(global-company-mode)
;; TODO key-bindings
;; semantic-ia-fast-jump
;; semantic-ia-describe-class
;; semanticdb-cleanup-cache-files
;; semantic-decoration-all-include-summary
;; semantic-analyze-proto-impl-toggle


(defalias 'yes-or-no-p 'y-or-n-p)

(ido-mode t)
(ido-ubiquitous-mode t)
;; Enable ido in dired commands
(put 'dired-do-copy   'ido nil)
(put 'dired-do-rename 'ido nil)

(eval-after-load "smex"
  '(smex-initialize))

;; wgrep-ag
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;; which-key
(eval-after-load "which-key"
  '(progn
     ;;(setq which-key-popup-type 'side-window)
     (setq which-key-side-window-location 'bottom)
     ;; (setq which-key-idle-delay 1.0)
     (setq which-key-max-description-length 50)
     (which-key-mode)))

;; aggressive-indent-mode
;; (eval-after-load "aggressive-indent"
;;   '(global-aggressive-indent-mode))

;; diff-hl
(eval-after-load "diff-hl"
  '(progn
     (setq diff-hl-draw-borders nil)
     ;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
     ;; (global-diff-hl-mode)
     ))

;; magit
(setq magit-popup-use-prefix-argument 'default)

;; desktop
(add-hook 'desktop-after-read-hook 'set-custom-frame-title)
(add-hook 'desktop-save-hook 'set-custom-frame-title)

;; flycheck
;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; (setq flycheck-completion-system 'ido)

;; gtags
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

;; make local variables not annoying
;; (setq enable-local-variables :all)

;; minibuffer
;; enable eldoc for minubuffer evaluation
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; faces
(set-face-attribute 'bold-italic nil :inherit '(bold italic))
(set-face-attribute 'italic nil :underline t)
(set-face-attribute 'woman-bold nil :inherit '(Man-overstrike))
(set-face-attribute 'woman-italic nil :inherit '(Man-underline))
;; the variable height fonts are annoyed
(eval-after-load "org"
  '(dolist (face '(org-level-1
                   org-level-2
                   org-level-3
                   org-level-4
                   org-level-5
                   org-level-6
                   org-level-7
                   org-level-8))
     (set-face-attribute face nil :height 'unspecified)))

;; MS Windows and diebold hacks
(when (eq system-type 'windows-nt)
  (load "w32-service")
  (load "windows"))

;; My keybindings
(load "keys")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
