(with-eval-after-load "grep"
  ;; add *.c to c++ aliases
  (setcdr (assoc "cc" grep-files-aliases) "*.cc *.cxx *.cpp *.[Cc] *.CC *.c++")
  (setcdr (assoc "cchh" grep-files-aliases) "*.cc *.[ch]xx *.[ch]pp *.[CHch] *.CC *.HH *.[ch]++")

  (add-to-list 'grep-find-ignored-files "TAGS*")

  (require 'wgrep nil t))

(global-set-key (kbd "C-M-g") 'rgrep)
