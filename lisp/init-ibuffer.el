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
                                      ((and group (file-directory-p group))
                                       group)
                                      (t default-directory)))))
       (list (read-file-name "Find file: " default-directory)
             t)))
    (find-file file wildcards))

  (add-hook 'ibuffer-hook #'ibuffer-set-filter-groups-by-project)
  (define-key ibuffer-mode-map [remap ibuffer-find-file] 'ibuffer-find-file+)
  (define-key ibuffer-mode-map (kbd "/ @") 'ibuffer-filter-by-project)


  ;; ibuffer configuration

  ;; Modified version of ibuffer column "name" with the uniquify part striped
  (define-ibuffer-column base-name
    (:name "Name"
           :inline t
           :header-mouse-map ibuffer-name-header-map
           :props
           ('mouse-face 'highlight 'keymap ibuffer-name-map
                        'ibuffer-name-column t
                        'help-echo '(if tooltip-mode
                                        "mouse-1: mark this buffer\nmouse-2: select this buffer\nmouse-3: operate on this buffer"
                                      "mouse-1: mark buffer   mouse-2: select buffer   mouse-3: operate"))
           :summarizer
           (lambda (strings)
             (let ((bufs (length strings)))
               (cond ((zerop bufs) "No buffers")
                     ((= 1 bufs) "1 buffer")
                     (t (format "%s buffers" bufs))))))
    (let* ((name (or (uniquify-buffer-base-name) (buffer-name)))
           (string (propertize name
                               'font-lock-face
                               (ibuffer-buffer-name-face buffer mark))))
      (if (not (seq-position string ?\n))
          string
        (replace-regexp-in-string
         "\n" (propertize "^J" 'font-lock-face 'escape-glyph) string))))

  (setq ibuffer-display-summary nil)
  (setq ibuffer-marked-char ?*)
  (setq ibuffer-modified-char ?M)
  (setq ibuffer-read-only-char ?R)

  (defun ibuffer-title-remove-underline (format)
    (ibuffer-assert-ibuffer-mode)
    (save-excursion
      (goto-line 2)
      (delete-region (point-at-bol) (+ (point-at-eol) 1))))

  (advice-add 'ibuffer-update-title-and-summary :after #'ibuffer-title-remove-underline)

  (setq ibuffer-formats
        '((mark modified read-only " "
                (base-name 60 60 :left :elide)
                " "
                (size 6 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 40 -1) " " filename)))

  (defun ibuffer-setup-hook ()
    (setq-local scroll-conservatively 0))

  ;; (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (add-hook 'ibuffer-mode-hook #'ibuffer-setup-hook))

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; default is list-buffers
(global-set-key (kbd "C-x 4 C-b") 'ibuffer-other-window)
