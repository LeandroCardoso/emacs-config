;; packages
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(comment-column 0)
 '(custom-buffer-done-kill t)
 '(custom-raised-buttons nil)
 '(delete-selection-mode t nil (delsel))
 '(ff-case-fold-search t)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t)
 '(global-subword-mode t)
 '(hi-lock-mode t t (hi-lock))
 '(highlight-nonselected-windows t)
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
 '(initial-scratch-message nil)
 '(ispell-query-replace-choices t)
 '(ispell-silently-savep t)
 '(jit-lock-stealth-time 1)
 '(mouse-avoidance-banish-position
   (quote
    ((frame-or-window . frame)
     (side . right)
     (side-pos . 0)
     (top-or-bottom . top)
     (top-or-bottom-pos . 0))))
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(nxml-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(sql-input-ring-file-name "~/.emacs.d/sql-history")
 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(which-function-mode t nil (which-func))
 '(winner-mode t)
)


;; Load all *.el files sorted by name at ~/.emacs.d/lisp. Sub-directories and files starting with
;; underline are ignored. If a compiled elisp file exist and it is not outdated, then load it
;; instead of the non-compiled one.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq load-prefer-newer t)
(mapc 'load (mapcar 'file-name-base
                    (directory-files (expand-file-name "lisp"
                                                       user-emacs-directory)
                                     nil
                                     "^[^_].*\\.el$")))

;; Semantic
(setq semantic-default-submodes '(global-semantic-highlight-func-mode
                                  global-semantic-idle-local-symbol-highlight-mode
                                  global-semantic-idle-scheduler-mode
                                  global-semantic-idle-summary-mode
                                  global-semanticdb-minor-mode))
(setq semantic-idle-work-parse-neighboring-files-flag t)
(setq semantic-idle-work-update-headers-flag t)
(setq semanticdb-project-root-functions
      (list
       (lambda (directory) (locate-dominating-file directory ".git"))
       (lambda (directory) (locate-dominating-file directory ".tfignore"))
       (lambda (directory) (locate-dominating-file directory "view.dat"))
       (lambda (directory) (locate-dominating-file directory ".dir-locals.el"))))
;; semantic imenu
(setq semantic-imenu-adopt-external-members nil) ;; put class members on t he first imenu level
(setq semantic-imenu-bucketize-file nil) ;; don't use buckets
;; (setq semantic-imenu-expand-type-members nil)
;; (setq semantic-imenu-index-directory t) ;; index the entire directory for tags
(setq semantic-imenu-summary-function 'semantic-format-tag-canonical-name) ;; see semantic-format-tag-functions

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

(show-paren-mode)

;;; enable abbrev-mode by default
(setq-default abbrev-mode t)


;; TODO key-bindings
;; semantic-ia-fast-jump
;; semantic-ia-describe-class
;; semanticdb-cleanup-cache-files
;; semantic-decoration-all-include-summary
;; semantic-analyze-proto-impl-toggle


(defalias 'yes-or-no-p 'y-or-n-p)

;; aggressive-indent-mode
;; (eval-after-load "aggressive-indent"
;;   '(global-aggressive-indent-mode))

;; flycheck
;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; (setq flycheck-completion-system 'ido)

;; gtags
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

;; Make local variables not annoying when loading .dir-locals.el. This is not recommended and I
;; should find a better to avoid the warning.
(setq enable-local-variables :all)

;; minibuffer
;; enable eldoc for minubuffer evaluation
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(setq tab-always-indent 'complete)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
