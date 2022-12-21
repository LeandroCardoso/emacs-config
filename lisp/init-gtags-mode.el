(when (require 'gtags-mode nil t)
  (require 'project)

  (defun gtags-mode-project-create ()
    "Create a GLOBAL GTAGS file in the root directory of the current
 project asynchronously.

When no project is found the `default-directory', ask the user
for a directory.

See `gtags-mode-create'."
    (interactive)
    (gtags-mode-create (project-root (project-current t))))

  (gtags-mode)

  ;; Force .h files to be treated as a C++
  (setenv "GTAGSFORCECPP" "1")

  (global-set-key (kbd "C-x p T") 'gtags-mode-update)
  (global-set-key (kbd "C-x p t") 'gtags-mode-project-create)

  ;; Use bash shell when calling global, because it fixes the annoying "^M" that can be displayed at
  ;; end of lines.
  (when (eq system-type 'windows-nt)
    (advice-add 'gtags-mode--exec-sync :around #'with-bash-shell)
    (advice-add 'gtags-mode--exec-async :around #'with-bash-shell)))
