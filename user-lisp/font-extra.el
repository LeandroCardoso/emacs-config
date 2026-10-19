;;; font-extra.el --- Extra font commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'faces)
(require 'frame)
(require 'help)

;; Internal functions
(defun list-ui-fonts (&optional proportional)
  "Return available UI font families that support Latin characters.

The returned list is sorted alphabetically and contains no duplicates.

By default, only monospaced font families are returned.  When
PROPORTIONAL is non-nil, include proportional font families as well."
  (seq-filter
   (lambda (family)
     (when-let* ((font (find-font (font-spec :family family))))
       (and
        ;; Basic Latin support
        (font-has-char-p font ?a)
        (or proportional
            ;; Monospace detection
            (let* ((adstyle (font-get font :adstyle))
                   (spacing (font-get font :spacing))
                   (finfo    (font-info family)))
              (or
               ;; Common on Windows
               (eq adstyle 'mono)
               ;; Common on Linux
               (eq spacing 100)
               ;; Fallback for fonts that do not report spacing like 'Noto Fonts', compare
               ;; average-width with max-width
               (and finfo
                    (= (aref finfo 7)
                       (aref finfo 11)))))))))
   (seq-uniq (sort (font-family-list) #'string<))))

;; Commands

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

;;;###autoload
(defun display-fonts (&optional show-all)
  "Display a buffer listing available Latin fonts.

By default, only monospaced fonts are shown.  With prefix argument
SHOW-ALL, display all fonts that contain Latin characters."
  (interactive "P")
  (let* ((help-buffer-under-preparation t)
         (sample-text
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 0123456789 `'\"~!@#$%^&*<>[]{}()_-+=/|\\.,;:")
         (fonts (list-ui-fonts show-all))
         ;; Compute the label width using the longest font name
         (label-width (if fonts (apply #'max (mapcar #'string-width fonts)) 0))
         (line-format (format "%%-%ds %%s\n" label-width)))
    (help-setup-xref (list #'display-fonts show-all) (called-interactively-p 'interactive))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
        (dolist (font fonts)
          (insert (format line-format font (propertize sample-text 'face `(:family ,font)))))))))

;;;###autoload
(defun set-frame-font+ (all-frames)
  "Set the font of the selected frame.

Prompt for a font name and apply it to the selected frame.

With prefix argument ALL-FRAMES, apply the font to all existing frames
and use it for future frames."
  (interactive "P")
  (set-frame-font (completing-read "Font name: " (list-ui-fonts) nil t) t all-frames))

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

(provide 'font-extra)

;;; font-extra.el ends here
