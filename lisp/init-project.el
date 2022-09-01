(with-eval-after-load 'project
  ;; settings
  (setq project-vc-merge-submodules nil)

  ;; functions
  (defun project-find-regexp-with-arg ()
    "Call `project-find-regexp' with \\[universal-argument] prefix."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'project-find-regexp)))

  ;; keys
  (global-set-key (kbd "C-x p C-g") 'project-find-regexp-with-arg))
