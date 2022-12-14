(when (require 'gtags-mode nil t)
  (gtags-mode)

  ;; Force .h files to be treated as a C++
  (setenv "GTAGSFORCECPP" "1")

  (global-set-key (kbd "C-x p t") 'gtags-mode-update)

  ;; Use bash shell when calling global, because it fixes the annoying "^M" that can be displayed at
  ;; end of lines.
  (when (eq system-type 'windows-nt)
    (advice-add 'gtags-mode--exec-sync :around #'with-bash-shell)
    (advice-add 'gtags-mode--exec-async :around #'with-bash-shell)))
