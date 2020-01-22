(with-eval-after-load "grep"
  (setq grep-save-buffers nil)

  ;; Remove c++ aliases and add new ones with *.c files in the begging of the list to get higher
  ;; priority.
  (assq-delete-all (car (assoc "cc" grep-files-aliases)) grep-files-aliases)
  (assq-delete-all (car (assoc "cchh" grep-files-aliases)) grep-files-aliases)
  (push '("cc" . "*.cc *.cxx *.cpp *.[Cc] *.CC *.c++") grep-files-aliases)
  (push '("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHch] *.CC *.HH *.[ch]++") grep-files-aliases)

  (add-to-list 'grep-files-aliases '("cs" . "*.cs"))

  (dolist (file '("TAGS*" "GPATH" "GRTAGS" "GTAGS" "*.exe"))
    (add-to-list 'grep-find-ignored-files file))

  (add-hook 'grep-setup-hook #'(lambda () (setq truncate-lines t)))

  (define-key grep-mode-map (kbd "r") 'rename-uniquely)
  (define-key grep-mode-map (kbd "k") 'keep-lines)
  (define-key grep-mode-map (kbd "f") 'flush-lines)

  (require 'wgrep nil t))

(global-set-key (kbd "C-M-g") 'rgrep)
