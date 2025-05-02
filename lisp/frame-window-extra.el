;;; frame-window-extra.el --- Extra frame and window commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'frame)
(require 'simple)
(require 'window)

;;;###autoload
(defun split-window-sensibly-horizontally (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.

WINDOW defaults to the currently selected window.

Replacement for `split-window-sensibly', but prefers
`split-width-threshold' over `split-height-threshold'."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
         ;; Split window vertically.
         (with-selected-window window
           (split-window-right)))
    (and (window-splittable-p window)
         ;; Split window horizontally.
         (with-selected-window window
           (split-window-below)))
    (and (eq window (frame-root-window (window-frame window)))
         (not (window-minibuffer-p window))
         ;; If WINDOW is the only window on its frame and is not the minibuffer window, try to split
         ;; it vertically disregarding the value of `split-height-threshold'.
         (let ((split-height-threshold 0))
           (when (window-splittable-p window)
         (with-selected-window window
           (split-window-below))))))))

;;;###autoload
(defun other-window-backward (count &optional all-frames)
  "Select another window in backward cyclic ordering of windows.

COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is positive,
skip COUNT windows backwards.  If COUNT is negative, skip -COUNT windows
forwards.  COUNT zero means do not skip any window, so select the
selected window.  In an interactive call, COUNT is the numeric prefix
argument.  Return nil.

If the `other-window' parameter of the selected window is a function and
`ignore-window-parameters' is nil, call that function with the arguments
COUNT and ALL-FRAMES."
  (interactive "p")
  (other-window (if (numberp count) (- count) count) all-frames))

;;;###autoload
(defun other-window-all-frames (count)
  "Select another window in cyclic ordering of windows in all frames.

COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is positive,
skip COUNT windows forwards.  If COUNT is negative, skip -COUNT windows
backwards.  COUNT zero means do not skip any window, so select the
selected window.  In an interactive call, COUNT is the numeric prefix
argument.  Return nil.

This function uses `other-window' with argument ALL-FRAMES:t for finding
the window to select."
  (interactive "p")
  (other-window count t)
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload
(defun other-window-all-frames-backward (count)
  "Select another window in backward cyclic ordering of windows in all frames.

COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is positive,
skip COUNT windows backwards.  If COUNT is negative, skip -COUNT windows
forwards.  COUNT zero means do not skip any window, so select the
selected window.  In an interactive call, COUNT is the numeric prefix
argument.  Return nil.

This function uses `other-window' with argument ALL-FRAMES:t for finding
the window to select."
  (interactive "p")
  (other-window-backward count t)
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload
(defun resize-window-to-region ()
  "Resize current window vertically to fit the size of the active region.

When region height is less than `window-min-width', it will be resized
to `window-min-height'."
  (interactive)
  (when (use-region-p)
    (let ((region-height (count-screen-lines (region-beginning)
                                             (if (and (bolp) (eq (point) (region-end)))
                                                 (1+ (region-end))
                                               (region-end)))))
      (window-resize nil (- (max window-min-height region-height) (window-body-height)))
    (recenter (count-lines (region-beginning) (point))))))

;;;###autoload
(defun kill-other-buffer-and-window (count)
  "Kill other buffer and window.

Select another window in cyclic ordering of windows.  COUNT specifies
the number of windows to skip, starting with the selected window, before
making the selection.  If COUNT is positive, skip COUNT windows
forwards.  If COUNT is negative, skip -COUNT windows backwards.  COUNT
zero means do not skip any window, so select the selected window.  In an
interactive call, COUNT is the numeric prefix argument.

See `kill-buffer' and `other-window'"
  (interactive "p")
  (save-selected-window
    (let ((curbuf (current-buffer)))
      (other-window count)
      (when (not (eq curbuf (current-buffer)))
        (kill-buffer-and-window)))))

;;;###autoload
(defun toggle-frame-fullscreen+ (arg)
  "Toggle fullscreen state of selected frame.

Make selected frame fullscreen or restore its previous size if it is
already fullscreen.

With parameter ARG, toggle selected frame state and toggle all the other
frames fullscreen state when their state is different from the selected
frame.

See also `toggle-frame-fullscreen'."
  (interactive "P")
  (if arg
      (let ((fullscreen (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))))
        (dolist (frame (frame-list))
          (when (and (frame-visible-p frame)
                     (eq (not fullscreen)
                         (not (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)))))
            (with-selected-frame frame
              (toggle-frame-fullscreen)))))
    (toggle-frame-fullscreen)))


;; From obsolete lucid.el
;;;###autoload
(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.

With a numeric parameter ARG, switch to the Nth most recent buffer.  ARG
0 means buries the current buffer at the bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth arg
      (apply 'nconc
         (mapcar
          (lambda (buf)
            (if (= ?\  (string-to-char (buffer-name buf)))
            nil
              (list buf)))
          (buffer-list)))))))

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
                  (eq 'mono (font-get (find-font (font-spec :family font)) :adstyle)))
          (if font-name-propertize
              (insert (propertize font 'face `(:family ,font)))
            (insert (substring font 0 (min (1- font-name-length) (length font)))))
          (insert (propertize " " 'display `(space :align-to ,font-name-length)))
          (insert (propertize text 'face `(:family ,font)))
          (newline)))))) 

(defcustom preferred-font-list nil
  "A list of preferred fonts to be set by `set-preferred-font'."
  :type '(repeat string)
  :group 'font-selection)

(defun set-preferred-font ()
  "Set the first font from `preferred-font-list' that is available in all frames."
  (require 'seq)
  (when-let ((font-name (seq-find (lambda (font) (find-font (font-spec :name font)))
                                  preferred-font-list)))
    (set-frame-font font-name t t)
    (message "Setting font to %s" font-name)))

(provide 'frame-window-extra)

;;; frame-window-extra.el ends here
