;;; nxml-context.el --- Display XML context in modeline and which-func -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'nxml-mode)

;; TODO integrate in `header-line-format' usign :eval

;; Adapted from https://www.emacswiki.org/emacs/NxmlMode
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
    (if (called-interactively-p t)
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

(define-minor-mode xml-context-mode
  nil
  :global t
  (require 'timer)
  (when (timerp xml-context-tree-update-timer)
    (cancel-timer 'xml-context-tree-update-timer))
  (setq xml-context-tree-update-timer nil)
  (when xml-context-mode
    (setq xml-context-tree-update-timer
          (run-with-idle-timer which-func-update-delay t 'xml-context-tree-update))))

;; which-func integration
;; See https://www.emacswiki.org/emacs/WhichFuncMode Non-standard languages (TL;DR;)
(defun nxml-which-func-setup ()
  (require 'which-func)
  (when (< (buffer-size) xml-context-maxout)
    (add-hook 'which-func-functions 'xml-context-path t t)))

(provide 'nxml-context)

;;; nxml-context.el ends here
