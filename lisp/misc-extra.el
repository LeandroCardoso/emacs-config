;;; misc-extra.el --- Miscellaneous utility commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'frame)
(require 'seq)
(require 'simple)
(require 'window)

;;;###autoload
(defun backward-page-smart (&optional count)
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.

After a successful page backward, move cursor to next line and recenter
it at the top position of the window.

A page boundary is any line whose beginning matches the regexp
`page-delimiter'.

See `backward-page'."
  (interactive)
  (forward-line -1)
  (backward-page count)
  (unless (bobp)
    (forward-line 1)
    (recenter 0)))

;;;###autoload
(defun forward-page-smart (&optional count)
  "Move forward to page boundary.  With arg, repeat, or go back if negative.

After a successful page forward, move cursor to next line and recenter
it at the top position of the window.

A page boundary is any line whose beginning matches the regexp
`page-delimiter'.

See `forward-page'."
  (interactive)
  (forward-page count)
  (unless (eobp)
    (forward-line 1)
    (recenter 0)))

;;;###autoload
(defun clean-buffer-list-check-idle-time-advice ()
  "Advice to avoid cleanup the buffer list when Emacs is idle.

Return t if Emacs idle time is less than the
`clean-buffer-list-delay-general'.

This function is intended to be used as an advice in `clean-buffer-list'
function:
  (advice-add \='clean-buffer-list :before-while \='clean-buffer-list-check-idle-time-advice)"
  (require 'midnight)
  (< (round (float-time (or (current-idle-time) '(0 0 0))))
     (* clean-buffer-list-delay-general 24 60 60)))

;;;###autoload
(defmacro define-other-window-command (command)
  "Define a version of COMMAND which execute in the another window.

The new function will be named \='COMMAND-other-window\='."
  `(defun ,(intern (concat (symbol-name command) "-other-window")) ();
     ,(format "Like `%s', but in other window." command)
     (interactive)
     (let ((display-buffer-overriding-action '((display-buffer-pop-up-window)
                                               (inhibit-same-window . t))))
       (call-interactively ',command))))

(define-derived-mode display-fonts-mode special-mode "Fonts"
  "Major mode used in the \"*fonts*\" buffer.")

;;;###autoload
(defun display-fonts (&optional only-mono)
  "Display a buffer with a list of all available fonts.

When ONLY-MONO parameter is non-nil, only display monospaced fonts."
  (interactive "P")
  (with-current-buffer-window "*fonts*" nil nil
    (let ((text "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 01213456789")
          (font-name-length 30)
          (font-name-propertize t))
      (dolist (font (seq-uniq (seq-sort #'string< (font-family-list))))
        (when (or (not only-mono)
                  ;; Linux reports spacing=100 and MS Windows reports adstyle=mono
                  (eq 'mono (font-get (find-font (font-spec :family font)) :adstyle))
                  (eq 100 (font-get (find-font (font-spec :family font)) :spacing)))
          (if font-name-propertize
              (insert (propertize font 'face `(:family ,font)))
            (insert (substring font 0 (min (1- font-name-length) (length font)))))
          (insert (propertize " " 'display `(space :align-to ,font-name-length)))
          (insert (propertize text 'face `(:family ,font)))
          (newline))))
    (display-fonts-mode)))

;;;###autoload
(defun insert-selected-window-thing-at-point ()
  "When in minibuffer, insert the thing at point of the selected windows.

Bind this command to a key in `minibuffer-local-map', the recommendation is \"M-.\"."
  (interactive)
  (when-let ((str (with-minibuffer-selected-window (thing-at-point 'symbol))))
    (insert str)))

;;;###autoload
(defun set-first-font (font-list)
  "Set the first font from FONT-LIST that is available in all frames."
  (when-let ((font-name (seq-find (lambda (font) (find-font (font-spec :name font)))
                                  font-list)))
    (set-frame-font font-name t t)
    (message "Setting font to %s" font-name)))

;;;###autoload
(defun smart-display-time-mode ()
  "Display a clock when Emacs is in fullscreen.

Enable `display-time-mode' if Emacs is running in a text teminal or if
it is running in a graphical display and any frame is in
fullscreen.  Disable it otherwise.

This function is intended to be used as an advice in
`toggle-frame-fullscreen' function:
  (advice-add \='toggle-frame-fullscreen :after \='smart-display-time-mode)"
  (require 'time)
  (display-time-mode
   (if (or (not (display-graphic-p))
           (seq-some #'(lambda (frame)
                         (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)))
                     (frame-list)))
       1 ; enable
     0))) ; disable

(defun switch-to-scratch-org ()
  "Switch to a buffer named \='scratch.org\='.

If it doesn't exist, create it in `org-mode'."
  (interactive)
  (switch-to-buffer (get-buffer-create "scratch.org"))
  (org-mode))

(provide 'misc-extra)

;;; misc-extra.el ends here
