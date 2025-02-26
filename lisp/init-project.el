(with-eval-after-load 'project
  ;; functions
  (defun project-info ()
    "Display the project instance of the current buffer."
    (interactive)
    (let ((pr (flatten-list (project-current))))
      (message "%s" (mapconcat 'prin1-to-string (nconc (butlast pr) (list ':) (last pr)) " "))))

  ;; Implementation based on project-find-regexp with universal-argument prefix. This version has a
  ;; workaround to use the default-directory, instead of the caller-dir when the caller-dir is not
  ;; inside of the default-directory, which is required to use this function as an option in the
  ;; project-switch-commands.
  (defun project-query-regexp (regexp)
    "Find all matches for REGEXP in the current project.

See `project-find-regexp'."
    (interactive (list (project--read-regexp)))
    (require 'xref)
    (require 'grep)
    (let* ((caller-dir default-directory)
           (pr (project-current t))
           (default-directory (project-root pr))
           (dir (read-directory-name "Base directory: "
                                     (if (file-in-directory-p caller-dir default-directory)
                                         caller-dir
                                       default-directory)
                                     nil t))
           (files (project--files-in-directory dir nil (grep-read-files regexp))))
      (xref-show-xrefs
       (apply-partially #'project--find-regexp-in-files regexp files)
       nil)))

  ;; settings
  (setq project-switch-use-entire-map t)
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
