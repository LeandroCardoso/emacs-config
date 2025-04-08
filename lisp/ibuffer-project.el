;;; ibuffer-project.el --- Integrate project.el with ibuffer.el -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'ibuffer)
(require 'project)
(require 'ibuf-ext)

;;; user options

(defcustom ibuffer-project-include-non-file nil
  "Whether non-file buffers are included.
Whether non-file buffers are included in the project groups when using
    the `ibuffer-set-filter-groups-by-project'"
  :type 'boolean
  :group 'ibuffer-project)

;;; variables

(defvar ibuffer-project-root-alist nil
  "Alist of buffers by project root directory.")

;;; functions

(defun ibuffer-project-generate-root-alist ()
  "Generate a list of buffers by project root directory.
Result is saved in `ibuffer-project-root-alist'."
  (setq ibuffer-project-root-alist nil)
  (dolist (buf (reverse (buffer-list)))
    (with-current-buffer buf
      (when (or ibuffer-project-include-non-file (ibuffer-buffer-file-name))
        (let ((pr (project-current)))
          (when pr
            (push (cons (buffer-name) (project-root pr))
                  ibuffer-project-root-alist)))))))

(define-ibuffer-filter project
    "Limit current view to buffers with project root directory matching QUALIFIER."
  (:description "project root directory"
                :reader (read-from-minibuffer "Filter by project root directory: "))
  (when-let
      (it (with-current-buffer buf
            (cdr (assoc (buffer-name) ibuffer-project-root-alist))))
    (equal qualifier it)))

(defun ibuffer-set-filter-groups-by-project ()
  "Set the current filter groups to filter by project root directory."
  (interactive)
  (ibuffer-project-generate-root-alist)
  (setq ibuffer-filter-groups
        (seq-uniq
         (mapcar (lambda (pr)
                   (cons (format "%s" (cdr pr)) `((project . ,(cdr pr)))))
                 ibuffer-project-root-alist)))
  (ibuffer-update nil t))

(defun ibuffer-find-file+ (file &optional wildcards)
  "Find file in project.
Like `ibuffer-find-file', but default to the directory of the buffer at
point or directory of the group at point when using the
`ibuffer-set-filter-groups-by-project'.

FILE and WILDCARDS are here only for compatibility with
`ibuffer-find-file'."
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

;;; hooks

(add-hook 'ibuffer-hook #'ibuffer-set-filter-groups-by-project)

;;; key bidings

(define-key ibuffer-mode-map [remap ibuffer-find-file] 'ibuffer-find-file+)
(define-key ibuffer-mode-map (kbd "/ @") 'ibuffer-filter-by-project)

(provide 'ibuffer-project)

;;; ibuffer-project.el ends here
