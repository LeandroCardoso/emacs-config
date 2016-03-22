(setq grep-find-template "find -L . <X> -type f <F> -exec grep <C> -nH -e <R> {} +")

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
  ;; MSVS
  (add-to-list 'grep-files-aliases '("msvs" . "*.sln *.vcxproj *.vcxproj.filters *.msbuild"))
  (add-to-list 'grep-files-aliases
               '("msvsch" . "*.h *.hpp *.hxx *.c *.cpp *.cxx *.sln *.vcxproj *.vcxproj.filters *.msbuild")))

(when (require 'wgrep nil t))
