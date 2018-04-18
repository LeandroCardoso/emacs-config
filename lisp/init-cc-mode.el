(setq-default c-basic-offset 4)

;; find-file
(setq cc-search-directories '("." "./*" "../*" "/usr/include" "/usr/local/include/*"))

;; Unfortunately it is really sad that people keep using .h for c++ header files instead of anything
;; more appropriate and I have to keep the following setting.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(with-eval-after-load "cc-mode"
  (add-to-list 'c-font-lock-extra-types "BOOL")
  (add-to-list 'c++-font-lock-extra-types "BOOL")

  ;; workaround for prog-mode-map
  (unless (keymap-parent c-mode-base-map)
    (set-keymap-parent c-mode-base-map prog-mode-map)))

;; hooks

(defun c-common-setup-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+))

(defun c-setup-hook ()
  (font-lock-add-keywords nil '(("\\<\\(TRUE\\|FALSE\\)\\>" . 'font-lock-constant-face))))

(defun c++-setup-hook ())

(add-hook 'c-mode-common-hook #'c-common-setup-hook)
(add-hook 'c-mode-hook #'c-setup-hook)
(add-hook 'c++-mode-hook #'c-setup-hook)
(add-hook 'c++-mode-hook #'c++-setup-hook)
