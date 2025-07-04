;;; xml-where.el --- Display XML parent elements in modeline and which-func -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'nxml-mode)
(require 'which-func)

;; TODO integrate in `header-line-format' using :eval

;; Adapted from https://www.emacswiki.org/emacs/NxmlMode
(defun xml-where-path ()
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

(defvar xml-where-maxout 1000000 ;1MB
  "Don't automatically compute the `which-func' and `xml-where-mode' if
buffer is this big or bigger.")
(defvar xml-where-tree-element-line-limit 1) ;TODO
(defvar xml-where-tree-show-on-screen nil) ;TODO wip
(defvar xml-where-tree-update-timer nil)

(defun xml-where-tree ()
  "Display the where tree of XML elements the point is on in the echo area."
  (interactive)
  (let ((visible-point (unless xml-where-tree-show-on-screen
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

(defun xml-where-tree-update ()
  (when (and (derived-mode-p 'nxml-mode)
             (< (buffer-size) xml-where-maxout))
    (xml-where-tree)))

(define-minor-mode xml-where-mode
  nil
  :global t
  :group 'nxml
  (require 'timer)
  (when (timerp xml-where-tree-update-timer)
    (cancel-timer xml-where-tree-update-timer))
  (setq xml-where-tree-update-timer nil)
  (when xml-where-mode
    (setq xml-where-tree-update-timer
          (run-with-idle-timer which-func-update-delay t 'xml-where-tree-update))))

;; which-func integration
;; See https://www.emacswiki.org/emacs/WhichFuncMode Non-standard languages (TL;DR;)
(defun xml-where-which-func-setup ()
  (when (< (buffer-size) xml-where-maxout)
    (add-hook 'which-func-functions 'xml-where-path 0 t)))

(provide 'xml-where)

;;; xml-where.el ends here
