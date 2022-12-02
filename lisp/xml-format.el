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
  "Remove an XML declaration at the beginning of buffer."
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

;; TODO keep highlight
(defun xml-display-embedded-other-window (element)
  "TODO"
  (interactive)
  (save-mark-and-excursion
    (let* ((xml-start-tag (format "<%s\\(?: [^>]*>\\|>\\)" element))
           (xml-end-tag (format "</%s>" element))
           (search-distance 15000)
           (case-fold-search t)
           (win-beg (save-excursion (move-to-window-line 0) (point)))
           (win-end (save-excursion (move-to-window-line -1) (end-of-line) (point)))
           (match-back-beg (save-excursion
                             (end-of-line)
                             (re-search-backward xml-start-tag (- (point) search-distance) t)))
           (match-back-end (when match-back-beg
                             (save-excursion
                               (goto-char match-back-beg)
                               (re-search-forward xml-end-tag (+ (point) search-distance) t))))
           (match-back-found (and match-back-end (> match-back-end win-beg)))
           (match-forw-beg (unless match-back-found
                             (save-excursion
                               (end-of-line)
                               (when (re-search-forward xml-start-tag win-end t)
                                 (match-beginning 0)))))
           (match-forw-end (when match-forw-beg
                             (save-excursion
                               (goto-char match-forw-beg)
                               (re-search-forward xml-end-tag (+ (point) search-distance) t))))
           (match-forw-found match-forw-end)
           (beg (if match-back-found
                    match-back-beg
                  (if match-forw-found
                      match-forw-beg)))
           (end (if match-back-found
                    match-back-end
                  (if match-forw-found
                      match-forw-end)))
           (content (when (and beg end) (buffer-substring-no-properties beg end))))
      (when content
        (with-current-buffer-window
            (format "*%s/%s*" element (or (uniquify-buffer-base-name) (buffer-name)))
            nil nil
          (insert content)
          (xml-mode)
          (xml-format))
        (pulse-momentary-highlight-region beg end)))))

(with-eval-after-load "nxml-mode"
  (define-key nxml-mode-map (kbd "C-c C-q") 'xml-format)
  (define-key nxml-mode-map (kbd "C-c C-M-x") 'xml-remove-declaration))

(provide 'xml-format)
