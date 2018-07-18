(with-eval-after-load "ibuffer"

  ;; ibuffer-project
  (require 'project)
  (defvar ibuffer-project-root-alist nil
    "Alist of cached buffers to project root directory.")

  (defun ibuffer-project-generate-root-alist ()
  (setq ibuffer-project-root-alist nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (ibuffer-buffer-file-name)
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

  (defun ibuffer-set-filter-groups-by-project-root ()
    "Set the current filter groups to filter by project root directory."
    (interactive)
    (ibuffer-project-generate-root-alist)
    (setq ibuffer-filter-groups
          (ibuffer-remove-duplicates
           (mapcar (lambda (pr)
                     (cons (format "%s" (cdr pr)) `((project . ,(cdr pr)))))
                   ibuffer-project-root-alist)))
    (ibuffer-update nil t))

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

  (add-hook 'ibuffer-hook #'ibuffer-set-filter-groups-by-project-root)
  ;; (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  )

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; default is list-buffers
(global-set-key (kbd "C-x 4 C-b") 'ibuffer-other-window)
