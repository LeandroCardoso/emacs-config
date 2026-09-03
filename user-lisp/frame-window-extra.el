;;; frame-window-extra.el --- Extra frame and window commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'frame)
(require 'simple)
(require 'window)

;;;###autoload
(defun display-frame-window-information ()
  "Display current frame and window width and height."
  (interactive)
  (message "Frame width:%d height:%d. Window width:%d height:%d."
           (frame-width) (frame-height)
           (window-width) (window-height)))

;;;###autoload
(defun window-split-dynamic-threshold-advice (func &rest args)
  "Thresholds used to check if the window may be split are set dynamically.

Set the value of `split-height-threshold' and `split-width-threshold'
dynamically considering the `frame-height' and `frame-width' when the
`window-combination-resize' is t.

FUNC and ARGS are the function to be advised and their arguments
respectively.

Usage - advise `window-splittable-p' function:
  (advice-add \='window-splittable-p :around \='window-split-dynamic-threshold-advice)"
  (let* ((max-h-windows (if split-width-threshold
                            (/ (frame-width) (/ split-width-threshold 2))
                          1))
         (max-v-windows (max (if split-height-threshold
                                 (/ (1- (frame-height)) (/ split-height-threshold 2))
                               1)
                             ;; force splitting horizontally when vertically is not possible
                             (if (= max-h-windows 1) 2 1)))
         (split-width-threshold (if (and split-width-threshold window-combination-resize)
                                    (- (/ (frame-width) (- max-h-windows 1)) 1)
                                  split-width-threshold))
         (split-height-threshold (if (and split-height-threshold window-combination-resize)
                                     (- (/ (frame-height) (- max-v-windows 1)) 1)
                                   split-height-threshold)))
    ;; DEBUG
    ;; (message "window-split-dynamic-threshold-advice width:%s height:%s max-h:%s max-v:%s"
    ;;          split-width-threshold split-height-threshold max-h-windows max-v-windows)
    (apply func args)))

;;;###autoload
(defun split-window-dwim (force)
  "Split the largest window in the current frame in two.

With parameter FORCE, ignores the `split-width-threshold' and
`split-height-threshold'."
  (interactive "P")
  (let* ((split-width-threshold (if force (* 2 window-min-width) split-width-threshold))
         (split-height-threshold (if force (* 2 window-min-height) split-height-threshold))
         (window (car (sort (window-list)
                           (lambda (a b)
                             (> (* (window-width a) (window-height a))
                                (* (window-width b) (window-height b)))))))
         (new-window (split-window-sensibly window)))
    (when new-window (select-window new-window))))

;;;###autoload
(defun other-window-split-if-single (count)
  "Select another window and split the window optionally.

Split the current window when it is the only window.

COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is positive,
skip COUNT windows forwards.  If COUNT is negative, skip -COUNT windows
backwards.  COUNT zero means do not skip any window, so select the
selected window.  In an interactive call, COUNT is the numeric prefix
argument.

See `other-window'."
  (interactive "p")
  (when (one-window-p)
    (split-window-sensibly))
  (other-window count))

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

See `kill-buffer' and `other-window'."
  (interactive "p")
  (save-selected-window
    (let ((curbuf (current-buffer)))
      (other-window count)
      (when (not (eq curbuf (current-buffer)))
        (kill-buffer-and-window)))))

;;;###autoload
(defun delete-other-window (count)
  "Delete other window.

Select another window in cyclic ordering of windows.  COUNT specifies
the number of windows to skip, starting with the selected window, before
making the selection.  If COUNT is positive, skip COUNT windows
forwards.  If COUNT is negative, skip -COUNT windows backwards.  COUNT
zero means do not skip any window, so select the selected window.  In an
interactive call, COUNT is the numeric prefix argument.

See `delete-window' and `other-window'"
  (interactive "p")
  (save-selected-window
    (let ((curbuf (current-buffer)))
      (other-window count)
      (when (not (eq curbuf (current-buffer)))
        (delete-window)))))

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

(provide 'frame-window-extra)

;;; frame-window-extra.el ends here
