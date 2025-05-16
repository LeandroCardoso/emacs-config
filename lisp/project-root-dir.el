;;; project-root-dir.el --- Project static root directories -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'find-func)
(require 'project)
(require 'seq)

(defcustom project-root-directory-list nil
  "List of directories that are a root directory of a project."
  :type '(repeat (string :tag "Directory"))
  :group 'project)

(defcustom project-root-up-directory-list nil
  "List of directories where each subdirectory is a root directory of a project."
  :type '(repeat (string :tag "Directory"))
  :group 'project)

(defun project-try-custom-root (filename)
  (let* ((exfilename (expand-file-name filename))
         (dir-regexp-list
          (nconc
           (mapcar #'(lambda (custom-root-dir)
                       (concat "^\\("
                               (regexp-quote (file-name-as-directory (expand-file-name custom-root-dir)))
                               "[^/]*/\\)"))
                   project-root-up-directory-list)
           (mapcar #'(lambda (custom-root-dir)
                       (concat "^\\("
                               (regexp-quote (file-name-as-directory (expand-file-name custom-root-dir)))
                               "\\)"))
                   project-root-directory-list)))
         (root (seq-some #'(lambda (dir-regexp)
                             (when (string-match dir-regexp exfilename)
                               (match-string 1 exfilename)))
                         dir-regexp-list)))
    (when root
      (cons 'static (abbreviate-file-name root)))))

(cl-defmethod project-root ((project (head static)))
  (cdr project))

(add-to-list 'project-find-functions 'project-try-custom-root)


;; Default directories

;; Add emacs lisp to its own project by default. Currently there is not way to get the emacs lisp
;; directory directly, so I am using a random library from emacs lisp directory.
(add-to-list 'project-root-directory-list (file-name-directory (find-library-name "misc")))

;; Add user's Emacs Lisp packages
(add-to-list 'project-root-up-directory-list (expand-file-name package-user-dir))

(provide 'project-root-dir)

;;; project-root-dir.el ends here
