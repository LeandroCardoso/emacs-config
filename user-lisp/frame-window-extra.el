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

(defun window-split-dynamic-threshold-advice (func &rest args)
  "Dynamically adjust window-splitting thresholds for `window-splittable-p'.

When `window-combination-resize' is non-nil, temporarily adjust
`split-width-threshold' and `split-height-threshold' based on the
current frame dimensions before calling `window-splittable-p'.

FUNC must be `window-splittable-p', ARGS are the arguments passed to
FUNC."
  (let* (;; Maximum number of windows that the configured width threshold would allow side by side.
         (max-horizontal-windows (if split-width-threshold
                                     (max 1
                                          (/ (frame-width)
                                             (/ split-width-threshold 2)))
                                   1))
         ;; Maximum number of windows that the configured height threshold would allow vertically
         ;; stacked.
         (max-vertical-windows (max (if split-height-threshold
                                        (/ (1- (frame-height))
                                           (/ split-height-threshold 2))
                                      1)
                                    ;; Ensure at least two vertical windows are possible when
                                    ;; horizontal splitting is not possible.
                                    (if (= max-horizontal-windows 1)
                                        2
                                      1)))
         (split-width-threshold (if (and split-width-threshold window-combination-resize)
                                    (1- (/ (frame-width)
                                           (max 1 (1- max-horizontal-windows))))
                                  split-width-threshold))
         (split-height-threshold (if (and split-height-threshold window-combination-resize)
                                     (1- (/ (frame-height)
                                            (max 1 (1- max-vertical-windows))))
                                   split-height-threshold)))
    ;; DEBUG
    ;; (message "window-split-dynamic-threshold-advice width:%s height:%s max-h:%s max-v:%s"
    ;;          split-width-threshold split-height-threshold max-h-windows max-v-windows)
    (apply func args)))

;;;###autoload
(defun window-split-dynamic-threshold-setup ()
  "Setup to adjust window-splitting thresholds for `window-splittable-p'.

See `window-split-dynamic-threshold-advice'."
 (advice-add #'window-splittable-p :around #'window-split-dynamic-threshold-advice))

;;;###autoload
(defun split-window-dwim ()
  "Split the largest window in the current frame in two.

When splitting, it ignores the `split-height-threshold'."
  (interactive)
  (let* ((split-height-threshold (* 2 window-min-height))
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
