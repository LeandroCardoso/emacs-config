(setq custom-file (concat user-emacs-directory "custom-variables.el"))
(setq gc-cons-threshold (* 32 1024 1024))

;; local packages
(let ((local-packages (expand-file-name "packages" user-emacs-directory)))
  (add-to-list 'load-path local-packages)
  (byte-recompile-directory local-packages 0))

(when (< emacs-major-version 27)
  (package-initialize))

;; Load all emacs list (*.el/*.elc) files sorted by name at ~/.emacs.d/lisp. Sub-directories and
;; files starting with underline or dot are ignored. If a compiled elisp file exist and it is not
;; outdated, then load it instead of the non-compiled one.
(setq load-prefer-newer t)
(let ((local-lisp (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path local-lisp)
  (byte-recompile-directory local-lisp 0)
  (mapc 'load (mapcar 'file-name-base (directory-files local-lisp nil "^[^_\\.].*\\.el$"))))

(setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
(message "emacs started in %s" (emacs-init-time))
