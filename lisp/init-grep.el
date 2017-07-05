(with-eval-after-load "grep"
  ;; delete the default c/c++ aliases
  (assq-delete-all (car(assoc "ch" grep-files-aliases)) grep-files-aliases)
  (assq-delete-all (car(assoc "c" grep-files-aliases)) grep-files-aliases)
  (assq-delete-all (car(assoc "cc" grep-files-aliases)) grep-files-aliases)
  (assq-delete-all (car(assoc "cchh" grep-files-aliases)) grep-files-aliases)
  (assq-delete-all (car(assoc "hh" grep-files-aliases)) grep-files-aliases)
  (assq-delete-all (car(assoc "h" grep-files-aliases)) grep-files-aliases)
  ;; c++ aliases
  (add-to-list 'grep-files-aliases '("h" . "*.h *.hpp *.hxx"))
  (add-to-list 'grep-files-aliases '("c" . "*.c *.cpp *.cxx"))
  (add-to-list 'grep-files-aliases '("ch" . "*.h *.hpp *.hxx *.c *.cpp *.cxx"))

  (grep-apply-setting 'grep-command "grep -I -nH ")
  (grep-apply-setting 'grep-find-command '("find -L . -type f -exec grep -I -nH  {} +" . 37))
  (grep-apply-setting 'grep-find-template "find -L . <X> -type f <F> -exec grep <C> -I -nH -e <R> {} +")

  (add-to-list 'grep-find-ignored-files "TAGS*")

  (global-set-key (kbd "C-c g") 'rgrep))


(require 'wgrep nil t)
