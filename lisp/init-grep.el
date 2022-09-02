(with-eval-after-load "grep"
  ;; settings
  (setq grep-save-buffers nil)

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

  (dolist (file '("TAGS*" "GPATH" "GRTAGS" "GTAGS"                           ;tags
                  "main.*.js" "polyfills.*.js" "runtime.*.js" "styles.*.css" ;minified files
                  "*.cache" "*.exe" "*.nupkg" "*.so" "*.zip"))
    (add-to-list 'grep-find-ignored-files file))

  (add-hook 'grep-setup-hook #'(lambda () (setq truncate-lines t)))

  ;; save grep regexp in kill-ring
  (defun grep-save-regexp-advice (regexp &rest r)
    "Save grep regexp into the kill-ring."
    (kill-new (car grep-regexp-history)))

  (advice-add 'lgrep :after #'grep-save-regexp-advice)
  (advice-add 'rgrep :after #'grep-save-regexp-advice)
  (advice-add 'zrgrep :after #'grep-save-regexp-advice)

  ;; keys
  (define-key grep-mode-map (kbd "u") 'rename-uniquely)
  (define-key grep-mode-map (kbd "k") 'keep-lines)
  (define-key grep-mode-map (kbd "f") 'flush-lines)

  (require 'wgrep nil t))

(global-set-key (kbd "C-M-g") 'rgrep)
