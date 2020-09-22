(with-eval-after-load 'project
  (defcustom project-simple-root-list nil
    "List of directories root projects.")

  (defcustom project-root-list nil
    "List of directories where every subdirectory is a root project.")

  (defun project-try-custom-root (filename)
    (let* ((exfilename (expand-file-name filename))
           (dir-regexp-list
            (nconc
             (mapcar #'(lambda (custom-root-dir)
                         (concat "^\\("
                                 (regexp-quote (file-name-as-directory (expand-file-name custom-root-dir)))
                                 "[^/]*/\\)"))
                     project-root-list)
             (mapcar #'(lambda (custom-root-dir)
                         (concat "^\\("
                                 (regexp-quote (file-name-as-directory (expand-file-name custom-root-dir)))
                                 "\\)"))
                     project-simple-root-list)))
           (root (seq-some #'(lambda (dir-regexp)
                               (when (string-match dir-regexp exfilename)
                                 (match-string 1 exfilename)))
                           dir-regexp-list)))
      (when root
        (cons 'dynamic (abbreviate-file-name root)))))

  (cl-defmethod project-roots ((project (head dynamic)))
    (list (cdr project)))

  (add-to-list 'project-find-functions 'project-try-custom-root)

  ;; Add emacs lisp to its own project by default. Currently there is not way to get the emacs lisp
  ;; directory directly, so I am using a random library from emacs lisp directory.
  (add-to-list 'project-simple-root-list (file-name-directory (find-library-name "misc")))

  (global-set-key (kbd "C-x C-M-f") 'project-find-file)
  (global-set-key (kbd "C-x C-M-g") 'project-find-regexp)
  )
