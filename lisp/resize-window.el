(defcustom resize-window-vertical-step 0.1
  "Default step for `enlarge-window+',
`enlarge-window-horizontally+', `shrink-window+' and
`shrink-window-horizontally+'. Given as a fraction of the current
frame height. The value should be a floating number between 0.1
and 0.9."
  :type 'float
  :group 'resize-window)

(defcustom resize-window-horizontal-step 0.1
  "Default step for `enlarge-window-horizontally+' and
`shrink-window-horizontally+'. Given as a fraction of the current
frame width. The value should be a floating number between 0.1
and 0.9."
  :type 'float
  :group 'resize-window)

(defun enlarge-window+ (delta &optional horizontal)
  "If DELTA is an integer, make the selected window DELTA lines taller.
If DELTA is a float or function was called interactively and no
argument is given, make the selected window
`resize-window-vertical-step' taller.

if optional argument HORIZONTAL is non-nil and DELTA is an
integer make selected window window wider by DELTA columns. If
optional argument HORIZONTAL is non-nil and DELTA is a float or
function was called interactively and no argument is given, make
selected window wider by `resize-window-horizontal-step'
columns.

if DELTA is negative, shrink selected window by -DELTA."
  (interactive "P")
  (if (integerp delta)
      (enlarge-window delta horizontal)
    (let* ((resize-window-step
            (if horizontal
                resize-window-horizontal-step
              resize-window-vertical-step))
           (step (truncate
                  (* (if horizontal (frame-width) (frame-height))
                     (cond ((floatp delta) delta)
                           ((eq delta '-) (- resize-window-step))
                           (t resize-window-step))))))
      (enlarge-window step horizontal))))

(defun shrink-window+ (delta &optional horizontal)
  "If DELTA is an integer, make the selected window DELTA lines smaller.
If DELTA is a float or function was called interactively and no
argument is given, make the selected window
`resize-window-vertical-step' smaller.

if optional argument HORIZONTAL is non-nil and DELTA is an
integer make selected window window narrower by DELTA columns. If
optional argument HORIZONTAL is non-nil and DELTA is a float or
function was called interactively and no argument is given, make
selected window narrower by `resize-window-horizontal-step'
columns.

if DELTA is negative, enlarge selected window by -DELTA."
  (interactive "P")
  (enlarge-window+ (cond ((numberp delta) (- delta))
                         ((eq delta '-) nil)
                         (t '-))
                   horizontal))

(defun enlarge-window-horizontally+ (delta)
  "If DELTA is an integer make selected window window wider by
DELTA columns. If DELTA is a float or function was called
interactively and no argument is given, make selected window
wider by `resize-window-horizontal-step' columns.

if DELTA is negative, shrink selected window by -DELTA."
  (interactive "P")
  (enlarge-window+ delta t))

(defun shrink-window-horizontally+ (delta)
  "If DELTA is an integer make selected window window narrower by
DELTA columns. If DELTA is a float or function was called
interactively and no argument is given, make selected window
narrower by `resize-window-horizontal-step' columns.

if DELTA is negative, shrink selected window by -DELTA."
  (interactive "P")
  (shrink-window+ delta t))

(global-set-key (kbd "C-]") 'enlarge-window+) ; default is abort-recursive-edit
(global-set-key (kbd "C-}") 'shrink-window+)
(global-set-key (kbd "C-M-]") 'enlarge-window-horizontally+)
(global-set-key (kbd "C-M-}") 'shrink-window-horizontally+)

(provide 'resize-window)
