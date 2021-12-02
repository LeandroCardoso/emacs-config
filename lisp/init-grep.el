(with-eval-after-load "grep"
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

  (define-key grep-mode-map (kbd "r") 'rename-uniquely)
  (define-key grep-mode-map (kbd "k") 'keep-lines)
  (define-key grep-mode-map (kbd "f") 'flush-lines)

  (require 'wgrep nil t))

(global-set-key (kbd "C-M-g") 'rgrep)
