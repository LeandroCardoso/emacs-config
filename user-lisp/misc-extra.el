;;; misc-extra.el --- Miscellaneous utility commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'midnight)

;;; Fonts
(defun list-ui-fonts (&optional proportional)
  "Return available UI font families that support Latin characters.

The returned list is sorted alphabetically and contains no duplicates.

By default, only monospaced font families are returned.  When
PROPORTIONAL is non-nil, include proportional font families as well."
  (seq-filter (lambda (font)
                (when-let* ((font-entity (find-font (font-spec :family font))))
                  ;; Ensure the font supports Latin characters
                  (and (font-has-char-p font-entity ?a)
                       ;; Linux commonly reports :spacing 100, while Windows often reports :adstyle
                       ;; mono
                       (or proportional
                           (eq (font-get font-entity :adstyle) 'mono)
                           (eq (font-get font-entity :spacing) 100)))))
              (seq-uniq (sort (font-family-list) #'string<))))

(defvar swap-fonts-pair nil "Pair of fonts used by `swap-fonts'.")

;;;###autoload
(defun swap-fonts (&optional reset)
  "Toggle between two fonts.

With prefix argument RESET, prompt for a new pair of fonts.  Otherwise,
toggle between the previously configured pair.

If no pair has been configured, prompt for one."
  (interactive "P")
  (when (or reset (null swap-fonts-pair))
    (let ((fonts (list-ui-fonts)))
      (setq swap-fonts-pair (cons (completing-read "First font: " fonts nil t)
                                  (completing-read "Second font: " fonts nil t)))))
  (setq swap-fonts-pair (cons (cdr swap-fonts-pair) (car swap-fonts-pair)))
  (message "Font: %s" (cdr swap-fonts-pair))
  (set-frame-font (cdr swap-fonts-pair) t))

;;;###autoload
(defun cycle-font (all-frames)
  "Switch to the next monospaced font that supports Latin characters.

With prefix argument ALL-FRAMES, apply the new font to all existing and
future frames."
  (interactive "P")
  (let* ((fonts (list-ui-fonts))
         (current-font (format "%s" (font-get (face-attribute 'default :font) :family)))
         (next-font (or (cadr (member current-font fonts))
                        (car fonts))))
    (message "Setting font to %s" next-font)
    (set-frame-font next-font t all-frames)))

(define-derived-mode display-fonts-mode special-mode "Fonts"
  "Major mode used in the \"*fonts*\" buffer.")

;;;###autoload
(defun display-fonts (&optional show-all)
  "Display a buffer listing available Latin fonts.

By default, only monospaced fonts are shown.  With prefix argument
SHOW-ALL, display all fonts that contain Latin characters."
  (interactive "P")
  (with-current-buffer-window "*fonts*" nil nil
    (let* ((sample-text
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 0123456789 `'\"~!@#$%^&*<>[]{}()_-+=/|\\.,;:")
           (fonts (list-ui-fonts show-all))
           ;; Compute the label width using the longest font name
           (label-width (if fonts (apply #'max (mapcar #'string-width fonts)) 0))
           (line-format (format "%%-%ds %%s\n" label-width)))
      (dolist (font fonts)
        (insert (format line-format font (propertize sample-text 'face `(:family ,font)))))
      (display-fonts-mode))))

;;;###autoload
(defun set-first-font (font-list)
  "Set the first font from FONT-LIST that is available in all frames."
  (when-let* ((font-name (seq-find (lambda (font) (find-font (font-spec :name font)))
                                   font-list)))
    (set-frame-font font-name t t)
    (message "Setting font to %s" font-name)))

;;;###autoload
(defun set-frame-font+ (all-frames)
  "Set the font of the selected frame.

Prompt for a font name and apply it to the selected frame.

With prefix argument ALL-FRAMES, apply the font to all existing frames
and use it for future frames."
  (interactive "P")
  (set-frame-font (completing-read "Font name: " (list-ui-fonts) nil t) t all-frames))


;;; Information

(defcustom display-system-misc-info nil
    "Additional information displayed by `display-system-info'.

Each element is a list of one of the following forms:

  (LABEL VALUE)
  (LABEL VALUE SHORT-LABEL)
  (LABEL VALUE SHORT-LABEL SHORT-VALUE)

LABEL and VALUE are used in detailed mode.

SHORT-LABEL and SHORT-VALUE customize the display in brief mode.  When
SHORT-LABEL is omitted, LABEL is used. When SHORT-VALUE is omitted,
VALUE is used.

If VALUE (or SHORT-VALUE) is a string, it is displayed verbatim.
Otherwise, it is treated as a boolean value and displayed as \"yes\"
when non-nil and \"no\" when nil."
    :type '(repeat
             (list
              (string :tag "Label")
              (sexp :tag "Value")
              (choice
               (const :tag "No short label" nil)
               (string :tag "Short label"))
              (choice
               (const :tag "No short label" nil)
               (sexp :tag "Short value"))))
  :group 'display)

;;;###autoload
(defun message-summary-data (detailed &rest fields)
  "Display a summary of FIELDS in the echo area.

Each element of FIELDS is either a field or a list of fields.

A field has one of the following forms:

  (LABEL VALUE)
  (LABEL VALUE SHORT-LABEL)
  (LABEL VALUE SHORT-LABEL SHORT-VALUE)

LABEL and VALUE are used in detailed mode.

SHORT-LABEL and SHORT-VALUE customize the display in brief mode.  When
SHORT-LABEL is omitted, LABEL is used. When SHORT-VALUE is omitted,
VALUE is used.

If VALUE (or SHORT-VALUE) is a string, it is displayed verbatim.
Otherwise, it is treated as a boolean value and displayed as \"yes\"
when non-nil and \"no\" when nil.

When DETAILED is non-nil, labels are shown and fields are separated by
newlines."
  (let* ((separator (if detailed "\n" " | "))
         ;; flatten the internal list struct, necessary to use the `display-system-misc-info'
         (fields (seq-mapcat (lambda (field)
                               (if (and (listp field)
                                        (listp (car field)))
                                   field
                                 (list field)))
                             fields))
         (label-width (when detailed
                        (apply #'max (mapcar (lambda (field) (string-width (car field))) fields)))))
    (message "%s"
             (mapconcat (pcase-lambda (`(,label ,value ,short-label, short-value))
                          (format (if detailed
                                      (format "%%-%ds %%s" label-width)
                                    "%s %s")
                                  (or (and (not detailed) short-label) label)
                                  (cond
                                   ((and (not detailed) (stringp short-value)) short-value)
                                   ((stringp value) value)
                                   ((or short-value value) "yes")
                                   (t "no"))))
                        fields
                        separator))))

(declare-function nerd-icons-devicon "nerd-icons")
(declare-function nerd-icons-faicon "nerd-icons")
(declare-function nerd-icons-flicon "nerd-icons")
(declare-function nerd-icons-sucicon "nerd-icons")
(declare-function nerd-icons-icon-for-os-release-id "nerd-icons-extra")

;;;###autoload
(defun display-monitor-layout-info (&optional detailed)
  "Display current monitor, frame and selected window dimensions.

With prefix argument DETAILED, display a detailed message, instead of a
brief one."
  (interactive "P")
  (let ((monitor-width (nth 2 (frame-monitor-attribute 'geometry)))
        (monitor-height (nth 3 (frame-monitor-attribute 'geometry))))
    (message-summary-data detailed
                          `("Monitor:" ,(format "%dx%d" monitor-width monitor-height))
                          `("Frame:" ,(format "%dx%d" (frame-width) (frame-height)))
                          `("Window:" ,(format "%dx%d" (window-width) (window-height))))))

;;;###autoload
(defun display-system-info (&optional detailed)
  "Display system information.

With prefix argument DETAILED, display a detailed message, instead of a
brief one."
  (interactive "P")
  (let* ((memory-info (mapcar #'(lambda (arg) (/ (float arg) (expt 1024 2))) (memory-info)))
         (system-icon (or (nerd-icons-icon-for-os-release-id (os-release-info "ID"))
                          (pcase system-type
                            ('gnu/linux (nerd-icons-flicon "nf-linux-tux"))
                            ('windows-nt (nerd-icons-devicon "nf-dev-windows"))
                            ('darwin (nerd-icons-devicon "nf-dev-apple"))))))
    (message-summary-data detailed
                          `("Emacs version:"
                            ,emacs-version
                            ,(nerd-icons-sucicon "nf-custom-emacs"))
                          `("System:"
                            ,(format "%s (%s)" (or (os-release-info "PRETTY_NAME") system-type) window-system)
                            ,system-icon
                            ,(format "%s (%s)" (or (os-release-info "NAME") system-type) window-system))
                          `("User:"
                            ,(format "%s | %s" user-login-name user-mail-address)
                            ,(nerd-icons-faicon "nf-fa-user")
                            ,user-login-name)
                          `("Hostname:" ,(system-name) ,(nerd-icons-faicon "nf-fa-desktop"))
                          display-system-misc-info
                          `("Uptime:"
                            ,(emacs-uptime)
                            ,(nerd-icons-faicon "nf-fa-clock")
                            ,(emacs-uptime "%D, %z%2h:%.2m"))
                          `("Load average:"
                            ,(apply #'format "%.2f %.2f %.2f" (load-average t))
                            ,(nerd-icons-faicon "nf-fa-microchip"))
                          `("Memory:"
                            ,(format "%.1f/%.1f GiB" (nth 1 memory-info) (nth 0 memory-info))
                            ,(nerd-icons-faicon "nf-fa-memory")
                            ,(format "%.1f GiB" (nth 1 memory-info)))
                          `("Started in:"
                            ,(emacs-init-time "%.2f seconds")
                            ,(nerd-icons-faicon "nf-fa-rocket")
                            ,(emacs-init-time "%.2fs")))))


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

(provide 'misc-extra)

;;; misc-extra.el ends here
