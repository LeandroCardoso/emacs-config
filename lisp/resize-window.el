;;; resize-window.el --- Extra resize window commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'window)

(defcustom resize-window-vertical-step 0.1
  "Default step for `enlarge-window+',
`enlarge-window-horizontally+', `shrink-window+' and
`shrink-window-horizontally+'. Value specifies either an
integer (the number of lines to enlarge or shrink the selected
window), a floating point number between 0.1 and 0.9 (the
fraction of the current window height)."
  :type 'float
  :group 'resize-window)

(defcustom resize-window-horizontal-step 0.1
  "Default step for `enlarge-window-horizontally+' and
`shrink-window-horizontally+'. Value specifies either an
integer (the number of columns to enlarge or shrink the selected
window), a floating point number between 0.1 and 0.9 (the
fraction of the current window width)."
  :type 'float
  :group 'resize-window)

;;;###autoload
(defun enlarge-window+ (delta &optional horizontal)
  "If DELTA is an integer, make the selected window DELTA lines taller.
If DELTA is a float between 0.1 and 0.9 make the selected window
the fraction of the current window height taller. If the function
was called interactively and no argument is given, make the
selected window `resize-window-vertical-step' taller.

If optional argument HORIZONTAL is non-nil and DELTA is an
integer make selected window wider by DELTA columns. If optional
argument HORIZONTAL is non-nil and DELTA is a float between 0.1
and 0.9 make the selected window the fraction of the current
window width wider. If the function was called interactively and
no argument is given, make selected window
`resize-window-horizontal-step' wider.

If DELTA is negative, shrink selected window by -DELTA."
  (interactive "P")
  (let* ((resize-window-step (if horizontal
                                 resize-window-horizontal-step
                               resize-window-vertical-step))
         (delta-or-default (cond ((eq delta '-) (- resize-window-step))
                                 ((null delta) resize-window-step)
                                 (t delta))))
    (enlarge-window (if (floatp delta-or-default)
                        (let ((dt-pxl (truncate (* (if horizontal (window-width) (window-height))
                                                  delta-or-default))))
                          (if (= dt-pxl 0) 1 dt-pxl))
                      delta-or-default)
                    horizontal)))

;;;###autoload
(defun shrink-window+ (delta &optional horizontal)
  "If DELTA is an integer, make the selected window DELTA lines shorter.
If DELTA is a float between 0.1 and 0.9 make the selected window
the fraction of the current window height shorter. If the
function was called interactively and no argument is given, make
the selected window `resize-window-vertical-step' shorter.

If optional argument HORIZONTAL is non-nil and DELTA is an
integer make selected window narrower by DELTA columns. If
optional argument HORIZONTAL is non-nil and DELTA is a float
between 0.1 and 0.9 make the selected window the fraction of the
current window width wider. if the function was called
interactively and no argument is given, make selected window
`resize-window-horizontal-step' narrower.

If DELTA is negative, enlarge selected window by -DELTA."
  (interactive "P")
  (enlarge-window+ (cond ((numberp delta) (- delta))
                         ((eq delta '-) nil)
                         (t '-))
                   horizontal))

;;;###autoload
(defun enlarge-window-horizontally+ (delta)
  "If DELTA is an integer make selected window wider by DELTA
columns. If DELTA is a float between 0.1 and 0.9 make the
selected window the fraction of the current window width
wider. If the function was called interactively and no argument
is given, make selected window `resize-window-horizontal-step'
wider.

If DELTA is negative, shrink selected window by -DELTA."
  (interactive "P")
  (enlarge-window+ delta t))

;;;###autoload
(defun shrink-window-horizontally+ (delta)
  "If DELTA is an integer make selected window narrower by DELTA
columns. If DELTA is a float between 0.1 and 0.9 make the
selected window the fraction of the current window width
narrower.  If the function was called interactively and no
argument is given, make selected window
`resize-window-horizontal-step' narrower.

If DELTA is negative, shrink selected window by -DELTA."
  (interactive "P")
  (shrink-window+ delta t))

(provide 'resize-window)

;;; resize-window.el ends here
