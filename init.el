
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file (concat user-emacs-directory "custom-variables.el"))

;; Load all *.el files sorted by name at ~/.emacs.d/lisp. Sub-directories and files starting with
;; underline are ignored. If a compiled elisp file exist and it is not outdated, then load it
;; instead of the non-compiled one.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq load-prefer-newer t)
(mapc 'load (mapcar 'file-name-base
                    (directory-files (expand-file-name "lisp" user-emacs-directory)
                                     nil
                                     "^[^_].*\\.el$")))

