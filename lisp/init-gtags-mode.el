(when (require 'gtags-mode nil t)
  (gtags-mode)

  ;; Force .h files to be treated as a C++
  (setenv "GTAGSFORCECPP" "1")

  ;; Use bash shell when calling global, because it doesn't play nice with the windows shell
  (when (eq system-type 'windows-nt)
    (advice-add 'gtags-mode--exec-sync :around #'execute-with-bash-shell-command-advice)
    (advice-add 'gtags-mode--exec-async :around #'execute-with-bash-shell-command-advice)))
