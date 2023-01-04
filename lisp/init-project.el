(with-eval-after-load 'project
  ;; settings
  (setq project-vc-merge-submodules nil)

  ;; functions
  (defun project-query-regexp ()
    "Find all matches for REGEXP in the current project.

See `project-find-regexp'."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'project-find-regexp)))

  ;; keys
  (global-set-key (kbd "C-x p q") 'project-query-regexp)
  (global-set-key (kbd "C-M-g") 'project-query-regexp))
