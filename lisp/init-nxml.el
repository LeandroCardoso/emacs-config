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
                               "--format" "--recover" "--nowarning" "--encode" "UTF-8" "-")))
    (error "xmllint executable not found")))

;; Original from https://www.emacswiki.org/emacs/NxmlMode
(defun xml-context-path ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path)
        (msg))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point))
                    (ignore-errors (progn (nxml-backward-up-element) t)))
          (setq path (cons (xmltok-start-tag-local-name) path)))))
    (setq msg (format "/%s" (mapconcat 'identity path "/")))
    (if (called-interactively-p)
        (message msg)
      msg)))

(defvar xml-context-maxout 1000000 ;1MB
  "Don't automatically compute the `which-func' and
`xml-context-mode' if buffer is this big or bigger.")
(defvar xml-context-tree-element-line-limit 1) ;TODO
(defvar xml-context-tree-show-on-screen nil) ;TODO wip
(defvar xml-context-tree-update-timer nil)

(defun xml-context-tree ()
  "Display the context tree of XML elements the point is on in the echo area."
  (interactive)
  (let ((visible-point (unless xml-context-tree-show-on-screen
                         (save-excursion (move-to-window-line 0) (point))))
        (path)
        (msg))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point))
                    (ignore-errors (progn (nxml-backward-up-element) t)))
          (when (and visible-point
                     (< (point) visible-point))
            (setq path (cons (buffer-substring (point) (nxml-token-after)) path))))))
    (when path
      (setq path (seq-map-indexed (lambda (elem index)
                                    (dotimes (_ index elem)
                                      (setq elem (concat "  " elem)))) path))
      (setq msg (format "%s" (mapconcat 'identity path "\n")))
      (message "%s" msg))))

(defun xml-context-tree-update ()
  (when (and (derived-mode-p 'nxml-mode)
             (< (buffer-size) xml-context-maxout))
    (xml-context-tree)))

(with-eval-after-load "nxml-mode"
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t)
  (define-key nxml-mode-map (kbd "C-c C-q") 'xml-format)
  (define-key nxml-mode-map (kbd "C-c C-c") 'xml-context-tree))

(define-minor-mode xml-context-mode
  nil :global t
  (when (timerp xml-context-tree-update-timer)
    (cancel-timer 'xml-context-tree-update-timer))
  (setq xml-context-tree-update-timer nil)
  (when xml-context-mode
    (setq xml-context-tree-update-timer
          (run-with-idle-timer idle-update-delay t #'xml-context-tree-update))))

;; which-func integration
;; See https://www.emacswiki.org/emacs/WhichFuncMode Non-standard languages (TL;DR;)
(defun nxml-which-func-setup-hook ()
  (when (and (derived-mode-p 'nxml-mode)
             (< (buffer-size) xml-context-maxout))
    (which-function-mode t)
    (setq which-func-mode t)
    (add-hook 'which-func-functions 'xml-context-path t t)))

;; We need this to be run AFTER which-func-ff-hook - the "t" means append
(add-hook 'find-file-hook 'nxml-which-func-setup-hook t)

;; TODO imenu - See libxml-parse-xml-region
;; TODO Use eldoc, instead of echo message
;; TODO show xml-context-tree in top of window. See how which-key do it in the bottom.
