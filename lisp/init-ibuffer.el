(with-eval-after-load "ibuffer"

  ;; ibuffer-project
  (require 'project)
  (require 'ibuf-ext)

  (defcustom ibuffer-project-include-non-file nil
    "Whether non-file buffers are included in the project groups
    when using the `ibuffer-set-filter-groups-by-project'")

  (defvar ibuffer-project-root-alist nil
    "Alist of cached buffers to project root directory.")

  (defun ibuffer-project-generate-root-alist ()
    (setq ibuffer-project-root-alist nil)
    (dolist (buf (reverse (buffer-list)))
      (with-current-buffer buf
        (when (or ibuffer-project-include-non-file (ibuffer-buffer-file-name))
          (let ((pr (project-current)))
            (when pr
              (push (cons (buffer-name) (car (project-roots pr)))
                    ibuffer-project-root-alist)))))))

  (define-ibuffer-filter project
      "Limit current view to buffers with project root directory matching QUALIFIER."
    (:description "project root directory"
     :reader (read-from-minibuffer "Filter by project root directory: "))
    (ibuffer-awhen
        (with-current-buffer buf
          (cdr (assoc (buffer-name) ibuffer-project-root-alist)))
      (equal qualifier it)))

  (defun ibuffer-set-filter-groups-by-project ()
    "Set the current filter groups to filter by project root directory."
    (interactive)
    (ibuffer-project-generate-root-alist)
    (setq ibuffer-filter-groups
          (ibuffer-remove-duplicates
           (mapcar (lambda (pr)
                     (cons (format "%s" (cdr pr)) `((project . ,(cdr pr)))))
                   ibuffer-project-root-alist)))
    (ibuffer-update nil t))

  (defun ibuffer-find-file+ (file &optional wildcards)
    "Like `find-file', but default to the directory of the buffer
at point or directory of the group at point when using the
`ibuffer-set-filter-groups-by-project'."
    (interactive
     (let ((default-directory (let ((buf (ibuffer-current-buffer))
                                    (group (get-text-property (point) 'ibuffer-filter-group-name)))
                                (cond ((buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory))
                                      ((file-directory-p group)
                                       group)
                                      (t default-directory)))))
       (list (read-file-name "Find file: " default-directory)
             t)))
    (find-file file wildcards))

  (add-hook 'ibuffer-hook #'ibuffer-set-filter-groups-by-project)
  (define-key ibuffer-mode-map [remap ibuffer-find-file] 'ibuffer-find-file+)
  (define-key ibuffer-mode-map (kbd "/ @") 'ibuffer-filter-by-project)


  ;;(setq ibuffer-display-summary nil)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 40 40 :left :elide)
                " "
                (size 6 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 40 -1) " " filename)))

  (defun ibuffer-setup-hook ()
    (setq-local scroll-conservatively 0))

  (add-hook 'ibuffer-mode-hook #'ibuffer-setup-hook)
  ;; (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  )

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; default is list-buffers
(global-set-key (kbd "C-x 4 C-b") 'ibuffer-other-window)
