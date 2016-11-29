;; packages
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-selected-packages '(zenburn-theme yasnippet which-key wgrep vc-tfs transpose-frame tfs solarized-theme smex smart-mode-line rainbow-mode monokai-theme moe-theme material-theme markdown-mode magit isearch-dabbrev irony idomenu ido-ubiquitous goto-chg flx-ido fic-mode dos diff-hl csharp-mode company-flx company))
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

;; pulse
(setq pulse-command-advice-flag t)

;; auto modes
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(eval-after-load "csharp"
  '(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))


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

;;; enable abbrev-mode by default
(setq-default abbrev-mode t)


;; TODO key-bindings
;; semantic-ia-fast-jump
;; semantic-ia-describe-class
;; semanticdb-cleanup-cache-files
;; semantic-decoration-all-include-summary
;; semantic-analyze-proto-impl-toggle


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
