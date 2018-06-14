(with-eval-after-load "grep"
  (setq grep-save-buffers nil)

  ;; add *.c to c++ aliases
  (setcdr (assoc "cc" grep-files-aliases) "*.cc *.cxx *.cpp *.[Cc] *.CC *.c++")
  (setcdr (assoc "cchh" grep-files-aliases) "*.cc *.[ch]xx *.[ch]pp *.[CHch] *.CC *.HH *.[ch]++")

  (dolist (file '("TAGS*" "GPATH" "GRTAGS" "GTAGS"))
    (add-to-list 'grep-find-ignored-files file))

  (require 'wgrep nil t))

(global-set-key (kbd "C-M-g") 'rgrep)
