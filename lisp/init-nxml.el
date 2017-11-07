(setq nxml-child-indent 4)
(setq nxml-slash-auto-complete-flag t)


(defun xml-pretty-print ()
  "Simple-minded pretty printer for XML.
Re-indents the XML and inserts newlines using xmllint (from
libxml2) tool.

This function try to respect the `indent-tabs-mode' and
`nxml-child-indent' variables and set the environment variable
XMLLINT_INDENT of the current buffer.
"
  (interactive "*")
  (make-local-variable 'process-environment)
  (setenv "XMLLINT_INDENT"
          (if indent-tabs-mode (make-string 1 9) (make-string nxml-child-indent 32)))
  (save-mark-and-excursion
   (call-process-region
    (point-min) (point-max)
    "xmllint"
    t
    '(t nil)
    nil
    "--format" "--recover" "--nowarning" "-")))

(with-eval-after-load "nxml-mode"
  (define-key nxml-mode-map (kbd "C-c p") 'xml-pretty-print))
