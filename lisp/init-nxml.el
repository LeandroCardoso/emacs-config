(defun xml-format ()
  "Format XML using xmllint (from libxml2).

If the region is active, act on region, otherwise act on the whole buffer.

Indentation is done using the `indent-tabs-mode' and
`nxml-child-indent' variables."
  (interactive "*")
  (if (executable-find "xmllint")
      (save-mark-and-excursion
        (make-local-variable 'process-environment)
        (setenv "XMLLINT_INDENT"
                (if indent-tabs-mode (make-string 1 9) (make-string nxml-child-indent 32)))
        (let ((min (if (use-region-p) (region-beginning) (point-min)))
              (max (if (use-region-p) (region-end) (point-max))))
          (call-process-region min max "xmllint" t '(t nil) nil
                               "--format" "--recover" "--nowarning" "-")))
    (error "xmllint executable not found")))

;; Original from https://www.emacswiki.org/emacs/NxmlMode
(defun xml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (ignore-errors (progn (nxml-backward-up-element) t)))
          (setq path (cons (xmltok-start-tag-local-name) path)))))
    (if (called-interactively-p)
        (message "/%s" (mapconcat 'identity path "/"))
      (format "/%s" (mapconcat 'identity path "/")))))

(with-eval-after-load "nxml-mode"
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t)
  (define-key nxml-mode-map (kbd "C-c C-q") 'xml-format))
;; which-func integration
;; See https://www.emacswiki.org/emacs/WhichFuncMode Non-standard languages (TL;DR;)
(defun nxml-which-func-setup-hook ()
  (when (and (derived-mode-p 'nxml-mode)
             (< (buffer-size) 1000000)) ;1MB
    (which-function-mode t)
    (setq which-func-mode t)
    (add-hook 'which-func-functions 'xml-where t t)))

;; We need this to be run AFTER which-func-ff-hook - the "t" means append
(add-hook 'find-file-hook 'nxml-which-func-setup-hook t)

