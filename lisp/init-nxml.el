(with-eval-after-load "nxml-mode"
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t)


  (defun xml-pretty-print (start end)
    "Simple-minded pretty printer for XML.
Re-indents the XML and inserts newlines using xmllint (from
libxml2) tool.

If the region is active, act on region, otherwise act on the whole buffer.

This function try to respect the `indent-tabs-mode' and
`nxml-child-indent' variables and set the environment variable
XMLLINT_INDENT of the current buffer.
"
    (interactive "*r")
    (if (executable-find "xmllint")
        (progn
          (make-local-variable 'process-environment)
          (setenv "XMLLINT_INDENT"
                  (if indent-tabs-mode (make-string 1 9) (make-string nxml-child-indent 32)))
          (save-mark-and-excursion
            (let ((min (if mark-active start (point-min)))
                  (max (if mark-active end (point-max))))
              (call-process-region min max "xmllint" t '(t nil) nil
                                   "--format" "--recover" "--nowarning" "-"))))
      (error "xmllint executable not found")))

  ;; Original from https://www.emacswiki.org/emacs/NxmlMode
  (defun nxml-where ()
    "Display the hierarchy of XML elements the point is on as a path."
    (interactive)
    (let ((path nil))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                      (condition-case nil
                          (progn
                            (nxml-backward-up-element) ; always returns nil
                            t)
                        (error nil)))
            (setq path (cons (xmltok-start-tag-local-name) path)))
          (if (called-interactively-p t)
              (message "/%s" (mapconcat 'identity path "/"))
            (format "/%s" (mapconcat 'identity path "/")))))))

  ;; (defun nxml-enable-which-func ()
  ;;   (when (derived-mode-p 'nxml-mode)
  ;;     (which-function-mode t)
  ;;     (setq which-func-mode t)
  ;;     (add-hook 'which-func-functions 'nxml-where t t)))

  ;; (add-hook 'find-file-hook 'nxml-enable-which-func t)

  (define-key nxml-mode-map (kbd "C-c p") 'xml-pretty-print))
