(setq nxml-child-indent 4)
(setq nxml-slash-auto-complete-flag t)

;; TODO use the region marked instead of the whole buffer.
(defun xml-pretty-print ()
  "Simple-minded pretty printer for XML.
Re-indents the code and inserts newlines.
You might want to turn on `auto-fill-mode' to get better results."
  (interactive)
  (require 'sgml-mode)
  (sgml-pretty-print (point-min) (point-max)))
