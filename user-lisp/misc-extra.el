;;; misc-extra.el --- Miscellaneous utility commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'midnight)

;;; Fonts
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
(defun set-first-font (font-list)
  "Set the first font from FONT-LIST that is available in all frames."
  (when-let* ((font-name (seq-find (lambda (font) (find-font (font-spec :name font)))
                                   font-list)))
    (set-frame-font font-name t t)
    (message "Setting font to %s" font-name)))


;;; Navigation

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
(defun switch-to-scratch-org ()
  "Switch to the scratch.org buffer.

If it doesn't exist, create it in `org-mode'."
  (interactive)
  (switch-to-buffer (get-buffer-create "scratch.org"))
  (org-mode))


;;; Midnight

;;;###autoload
(defun clean-buffer-list-check-idle-time-advice ()
  "Advice to avoid cleanup the buffer list when Emacs is idle.

Return t if Emacs idle time is less than the
`clean-buffer-list-delay-general'.

This function is intended to be used as an advice in `clean-buffer-list'
function:
  (advice-add \='clean-buffer-list :before-while \='clean-buffer-list-check-idle-time-advice)"
  (< (round (float-time (or (current-idle-time) '(0 0 0))))
     (* clean-buffer-list-delay-general 24 60 60)))


;;; Minibuffer

;;;###autoload
(defun smart-display-time-mode ()
  "Display a clock when Emacs is in fullscreen.

Enable `display-time-mode' if Emacs is running in a text teminal or if
it is running in a graphical display and any frame is in
fullscreen.  Disable it otherwise.

This function is intended to be used as an advice in
`toggle-frame-fullscreen' function:
  (advice-add \='toggle-frame-fullscreen :after \='smart-display-time-modegpnpnp)"
  (require 'time)
  (display-time-mode
   (if (or (not (display-graphic-p))
           (seq-some #'(lambda (frame)
                         (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)))
                     (frame-list)))
       1 ; enable
     0)))

;;;###autoload
(defun yank-symbol-at-point-into-minibuffer ()
  "Pull next symbol from buffer into minibuffer..

Bind this command to a key in `minibuffer-local-map', the recommendation
is \"C-M-w\"."
  (interactive)
  (when-let* ((str (with-minibuffer-selected-window (thing-at-point 'symbol t))))
    (insert str)))

;;;###autoload
(defun yank-word-at-point-into-minibuffer ()
  "Pull next word from buffer into minibuffer..

Bind this command to a key in `minibuffer-local-map', the recommendation
is \"C-w\"."
  (interactive)
  (when-let* ((str (with-minibuffer-selected-window (thing-at-point 'word t))))
    (insert str)))


;;; System

;;;###autoload
(defun os-release-info (parameter)
  "Return the operating system release (os-release) PARAMETER value."
  (when (file-readable-p "/etc/os-release")
    (with-temp-buffer
      (insert-file-contents "/etc/os-release")
      (when (re-search-forward (format "^%s=\\(.+\\)$" parameter) nil t)
        (string-trim (match-string-no-properties 1) "\"" "\"")))))

(declare-function nerd-icons-devicon "nerd-icons")
(declare-function nerd-icons-faicon "nerd-icons")
(declare-function nerd-icons-flicon "nerd-icons")
(declare-function nerd-icons-sucicon "nerd-icons")
(declare-function nerd-icons-icon-for-os-release-id "nerd-icons-extra")

;;;###autoload
(defun message-summary-data (fields &optional detailed)
  "Display a summary of data FIELDS in the echo area.

Each element of FIELDS must be a list of the form:

  (LABEL ICON VALUE)

When DETAILED is non-nil, labels are displayed and fields are separated
by newlines.  Otherwise, icons are displayed when available and
`nerd-icons' is loaded.

If VALUE is a string, it is displayed verbatim next to the label or
icon.  Otherwise, VALUE is treated as a boolean and rendered as \"yes\"
when non-nil and \"no\" when nil."
  (let* ((icons-enabled-p (featurep 'nerd-icons))
         (separator (if detailed "\n" " | ")))
    (message
     "%s"
     (mapconcat
      (pcase-lambda (`(,label ,icon ,value))
        (format "%s %s"
                (if (or detailed (not icons-enabled-p) (not icon))
                    label
                  icon)
                (cond
                 ((stringp value) value)
                 (value "yes")
                 (t "no"))))
      fields
      separator))))

;;;###autoload
(defun display-frame-window-information (&optional detailed)
  "Display current frame and selected window dimensions.

With prefix argument DETAILED, display a detailed message, instead of a
brief one."
  (interactive "P")
  (let* ((frame-size (list "Frame:"
                           nil
                           (format "%dx%d" (frame-width) (frame-height))))
         (window-size (list "Window:"
                            nil
                            (format "%dx%d" (window-width) (window-height))))
         (fields (list frame-size window-size)))
    (message-summary-data fields detailed)))

;;;###autoload
(defun display-system-information (&optional detailed)
  "Display system information.

With prefix argument DETAILED, display a detailed message, instead of a
brief one."
  (interactive "P")
  (let* ((version (list "Emacs version:"
                        (nerd-icons-sucicon "nf-custom-emacs")
                        emacs-version))
         (system-value (format "%s (%s)"
                               (or (if detailed
                                       (os-release-info "PRETTY_NAME")
                                     (os-release-info "NAME"))
                                   system-type)
                               window-system))
         (system-icon (or (nerd-icons-icon-for-os-release-id (os-release-info "ID"))
                          (pcase system-type
                            ('gnu/linux (nerd-icons-flicon "nf-linux-tux"))
                            ('windows-nt (nerd-icons-devicon "nf-dev-windows"))
                            ('darwin (nerd-icons-devicon "nf-dev-apple")))))
         (system (list "System:"
                       system-icon
                       system-value))
         (user (list "User:"
                     (nerd-icons-faicon "nf-fa-user")
                     user-login-name))
         (hostname (list "Hostname:"
                         (nerd-icons-faicon "nf-fa-desktop")
                         (system-name)))
         (rdi (list "RDI:"
                    (nerd-icons-faicon "nf-fa-burger")
                    rdi-p))
         (wsl (list "WSL:"
                    (nerd-icons-devicon "nf-dev-windows")
                    wsl-p))
         (uptime (list "Uptime:"
                       (nerd-icons-faicon "nf-fa-clock" )
                       (emacs-uptime (unless detailed "%D, %z%2h:%.2m"))))
         (load-avg (list "Load average:"
                         (nerd-icons-faicon "nf-fa-microchip")
                         (apply #'format "%.2f %.2f %.2f" (load-average t))))
         (init-time (list "Started in"
                          (nerd-icons-faicon "nf-fa-rocket")
                          (emacs-init-time (if detailed "%.2f seconds" "%.2fs"))))
         (fields (list version system user hostname rdi wsl uptime load-avg init-time)))
    (message-summary-data fields detailed)))

(provide 'misc-extra)

;;; misc-extra.el ends here
