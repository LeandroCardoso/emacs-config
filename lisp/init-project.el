(with-eval-after-load 'project
  (defvar project-root-list nil
    "List of directories, where each directory is the root of some project.
All subdirectories of a root project are considered a part of one project.")

  (defun project-try-root-dir (dir)
    (let ((root (seq-find #'(lambda (pr)
                              (and (> (length dir) (length pr))
                                   (string-prefix-p pr dir)))
                          (mapcar 'file-name-as-directory
                                  (mapcar 'expand-file-name project-root-list)))))
      (when root
        (cons 'root-dir
              (abbreviate-file-name (substring dir 0 (string-match "/" dir (length root))))))))

  (cl-defmethod project-roots ((project (head root-dir)))
    (list (cdr project)))

  (add-to-list 'project-find-functions 'project-try-root-dir)

  (global-set-key (kbd "C-x C-M-f") 'project-find-file)
  (global-set-key (kbd "C-x C-M-g") 'project-find-regexp)
  )
