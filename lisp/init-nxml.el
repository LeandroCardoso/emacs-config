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
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(with-eval-after-load "nxml-mode"
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t)
  (define-key nxml-mode-map (kbd "C-c C-q") 'xml-format))

;; (defun nxml-enable-which-func ()
;;   (when (derived-mode-p 'nxml-mode)
;;     (which-function-mode t)
;;     (setq which-func-mode t)
;;     (add-hook 'which-func-functions 'nxml-where t t)))

;; (add-hook 'find-file-hook 'nxml-enable-which-func t)
