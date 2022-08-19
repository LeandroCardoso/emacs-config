(setq custom-file (concat user-emacs-directory "custom-variables.el"))
(setq gc-cons-threshold (* 32 1024 1024))

;; local packages
(let ((local-packages (expand-file-name "packages" user-emacs-directory)))
  (add-to-list 'load-path local-packages)
  (byte-recompile-directory local-packages 0))

(when (< emacs-major-version 27)
  (package-initialize))

(setq load-prefer-newer t)
(let ((local-lisp (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path local-lisp)

  ;; Compile all elisp source files (.el)
  (message "Compiling elisp source files from %s" local-lisp)
  (byte-recompile-directory local-lisp 0)

  ;; Delete old elisp compiled files (.elc) that doesn't have a eslisp source file (.el) associated
  (message "Deleting unused elisp compiled files from %s" local-lisp)
  (dolist (file (directory-files local-lisp nil ".*\\.elc$"))
    (unless (file-exists-p (expand-file-name (file-name-with-extension file ".el") local-lisp))
      (message "Deleting %s" file)
      (delete-file (expand-file-name file local-lisp))))

  ;; Load all elisp files sorted by name. Sub-directories and files starting with underline or dot
  ;; are ignored
  (message "Loading elisp source files from %s" local-lisp)
  (dolist (file (directory-files local-lisp nil "^[^_\\.].*\\.el$"))
    (load (file-name-sans-extension file))))

(setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
(message "emacs started in %s" (emacs-init-time))
