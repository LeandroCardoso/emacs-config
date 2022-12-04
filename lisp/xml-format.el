(require 'env)
(require 'files)
(require 'nxml-mode)

(defcustom xml-format-declaration 'keep
  "Wheter `xml-format' will keep, remove or insert an XML
declaration in the first line from the formatted XML."
  :type '(choice (const :tag "Keep an XML declaration when present in original XML" keep)
                 (const :tag "Remove the XML declaration in the formatted XML" nil)
                 (const :tag "Insert an XML declaration in the formatted XML") t)
  :group 'xml-format)

(defun xml-declaration-p (&optional begin)
  "Return t when the buffer begins with a XML declaration."
  (save-mark-and-excursion
    (goto-char (or begin (point-min)))
    (looking-at-p "<\\?xml")))

(defun xml-remove-declaration ()
  "Remove an XML declaration from the beginning of buffer."
  (interactive "*")
  (goto-char (point-min))
  (when (looking-at-p "^<\\?xml")
    (kill-region (point) (min (1+ (point-at-eol)) (point-max)))))

(defun xml-format ()
  "Format an XML buffer or region using xmllint (from libxml2).

If the region is active, act on region, otherwise act on the
whole buffer.

Indentation is done using the `indent-tabs-mode' and
`nxml-child-indent' variables.

An XML declaration in the first line can be present in the
formatted XML according with the `xml-format-declaration'.

Return t when buffer was modified."
  (interactive "*")
  (unless (executable-find "xmllint")
    (error "xmllint executable not found"))
  (make-local-variable 'process-environment)
  (setenv "XMLLINT_INDENT"
          (if indent-tabs-mode (make-string 1 9) (make-string nxml-child-indent 32)))
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (decl (xml-declaration-p beg))
         (buf (generate-new-buffer "* temp output*"))
         (result nil))
    (when (> end beg) ;only continue when buffer or region is not empty
      (call-process-region beg end "xmllint" nil (list buf nil) nil
                           "--format" "--recover" "--nowarning" "--encode" "UTF-8" "-")
      (with-current-buffer buf
        ;; Remove the XML declaration inserted by the xmllint
        (xml-remove-declaration)
        ;; result is t when output buffer is not empty
        (setq result (> (buffer-size) 1))
        ;; Restore the XML declaration after set the result
        (when (or (eq xml-format-declaration nil)
                  (and decl (eq xml-format-declaration 'keep)))
          (nxml-insert-xml-declaration)))
      (if result
          (progn
            (replace-region-contents beg end (lambda () buf))
            ;; Add a new line when region does not begin on the beginning of the line
            (save-mark-and-excursion
              (goto-char beg)
              (when (> beg (point-at-bol))
                (insert "\n"))))
        (message "XML invalid"))
      (kill-buffer buf))
    result))

(with-eval-after-load "nxml-mode"
  (define-key nxml-mode-map (kbd "C-c C-q") 'xml-format)
  (define-key nxml-mode-map (kbd "C-c C-M-x") 'xml-remove-declaration))

(provide 'xml-format)
