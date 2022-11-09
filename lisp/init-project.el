(with-eval-after-load 'project
  ;; settings
  (setq project-vc-merge-submodules nil)

  ;; functions
  (defun project-find-regexp-in-directory ()
    "Like `project-find-regexp', but you is always asked for a directory to search in."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'project-find-regexp)))

  ;; keys
  (global-set-key (kbd "C-M-g") 'project-find-regexp)
  (global-set-key (kbd "C-x p M-g") 'project-find-regexp-in-directory)
  (global-set-key (kbd "C-M-G") 'project-find-regexp-in-directory))
