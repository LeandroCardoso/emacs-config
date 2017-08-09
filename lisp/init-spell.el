;; ispell
(setq ispell-dictionary "en_US")
(setq ispell-help-in-bufferp 'electric)
(setq ispell-program-name "hunspell")
(setq ispell-query-replace-choices t)
(setq ispell-silently-savep t)

(when (eq system-type 'windows-nt)
  ;; 'look' is not automatically used because we have a custom unix path in windows.
  (setq ispell-look-p t)
  ;; use custom user dictionary in windows
  (setq ispell-complete-word-dict (expand-file-name "~/dict/en_US.txt"))
  (setenv "DICPATH" (expand-file-name "~/dict"))
  (setenv "DICTIONARY" "en_US"))


;; flyspell
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-persistent-highlight nil) ;; make flyspell less annoying
(setq flyspell-mode-line-string nil)
(setq flyspell-use-meta-tab nil)

;; Redefine flyspell-mode-map, I hate the default keybindings.
(setq flyspell-mode-map (make-sparse-keymap))
(define-key flyspell-mode-map (kbd "C-$") 'flyspell-auto-correct-word) ;; C-$ is similar to M-$.

;; enable flyspell
;; nXML mode is crashing emacs so it is disabled as a workaround
(add-hook 'text-mode-hook #'(lambda () (unless (string= mode-name "nXML") (flyspell-mode))))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
