;; Required by Package.el
;; (package-initialize)

(setq custom-file (concat user-emacs-directory "custom-variables.el"))

;; Load all emacs list (*.el/*.elc) files sorted by name at ~/.emacs.d/lisp. Sub-directories and
;; files starting with underline or dot are ignored. If a compiled elisp file exist and it is not
;; outdated, then load it instead of the non-compiled one.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq load-prefer-newer t)
(mapc 'load (mapcar 'file-name-base
                    (directory-files (expand-file-name "lisp" user-emacs-directory)
                                     nil
                                     "^[^_\\.].*\\.el$")))

