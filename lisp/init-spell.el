;; ispell
(with-eval-after-load "ispell"
  (setq ispell-dictionary "en_US")
  (setenv "DICTIONARY" ispell-dictionary)
  (setq ispell-help-in-bufferp 'electric)
  (setq ispell-personal-dictionary
        (expand-file-name (concat user-emacs-directory "dict_" ispell-dictionary)))
  (setq ispell-program-name "hunspell")
  (setq ispell-query-replace-choices t)
  (setq ispell-silently-savep t)

  (let ((word-dict (expand-file-name (concat user-emacs-directory "words.txt"))))
    (when (file-exists-p word-dict)
      (setq ispell-complete-word-dict word-dict)))

  (when (eq system-type 'windows-nt)
    ;; 'look' is not automatically used because we have a custom unix path in windows.
    (setq ispell-look-p t)))

;; flyspell
(with-eval-after-load "flyspell"
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-persistent-highlight nil) ;; make flyspell less annoying
  (setq flyspell-mode-line-string nil)
  (setq flyspell-use-meta-tab nil)

  ;; Redefine flyspell-mode-map, I hate the default keybindings.
  (setq flyspell-mode-map (make-sparse-keymap))
  (define-key flyspell-mode-map (kbd "C-$") 'flyspell-auto-correct-word) ;; C-$ is similar to M-$.
  )

;; enable flyspell
(unless (eq system-type 'windows-nt)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))
