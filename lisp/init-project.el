(with-eval-after-load 'project
  ;; functions
  (defun project-info ()
    "Display the project instance of the current buffer."
    (interactive)
    (let ((pr (flatten-list (project-current))))
      (message "%s" (mapconcat 'prin1-to-string (nconc (butlast pr) (list ':) (last pr)) " "))))

  (defun project-query-regexp ()
    "Find all matches for REGEXP in the current project.

See `project-find-regexp'."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'project-find-regexp)))

  ;; settings
  (setq project-vc-merge-submodules nil)

  ;; Add project-query-regexp, when it is not present, to the project-switch-project after the
  ;; project-find-regexp command
  (unless (member '(project-query-regexp "Query regexp") project-switch-commands)
    (push '(project-query-regexp "Query regexp")
          (cdr (member '(project-find-regexp "Find regexp") project-switch-commands))))

  ;; keys
  (define-key project-prefix-map "i" 'project-info)
  (define-key project-prefix-map "q" 'project-query-regexp)
  (global-set-key (kbd "C-M-g") 'project-query-regexp))
