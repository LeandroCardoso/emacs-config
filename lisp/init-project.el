(with-eval-after-load 'project
  ;; Workaround for emacs 27.1 - Emacs 27.1 does not treat a Git submodule as a project root.
  ;; Unfortunately I need this functionality, so I am restoring the 26.3 implementation until 28.1
  ;; when an user option is been introduced.
  (defun project-try-vc-simple (dir)
    (let* ((backend (ignore-errors (vc-responsible-backend dir)))
           (root (and backend (ignore-errors
                                (vc-call-backend backend 'root dir)))))
      (and root (cons 'vc root))))

  (add-to-list 'project-find-functions 'project-try-vc-simple)

  ;; Custom directory project

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

  ;; Add user's Emacs Lisp packages
  (add-to-list 'project-root-list (expand-file-name package-user-dir))

  (global-set-key (kbd "C-x C-M-f") 'project-find-file)
  (global-set-key (kbd "C-x C-M-g") 'project-find-regexp))
