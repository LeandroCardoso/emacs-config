(require 'faces)
(require 'frame)
(require 'seq)

;; variables

(defvar preferred-font-list '("Source Code Pro" "Cascadia Mono" "Consolas")
  "A list of preferred fonts.")

(defvar font-size-reference '(96 . 10)
  "Reference font size.

Value has the form (DPI . POINT-SIZE).")

(defvar font-size-minimum 8
  "Mininum font size.")

;; functions

;; | resolution | diagonal | mm-size |    dpi |
;; |  1280x1024 |       19 | 377x302 |  86.27 |
;; |  1920x1080 |       14 | 310x174 | 157.35 |
;; |  1920x1080 |       23 | 510x287 |  95.78 |
;; |  2560x1440 |       25 | 553x311 | 117.49 |
(defun frame-monitor-dpi (&optional frame x y)
  "Return the dpi (dots-per-inch) of FRAME's monitor.

If FRAME is omitted or nil, use currently selected frame.

By default, the current monitor is said to be the physical
monitor dominating the selected frame. A frame is dominated by a
physical monitor when either the largest area of the frame
resides in the monitor, or the monitor is the closest to the
frame if the frame does not intersect any physical monitors.

If X and Y are both numbers, then ignore the value of FRAME; the
monitor is determined to be the physical monitor that contains
the pixel coordinate (X, Y).

See `frame-monitor-attribute'."
  (/ (nth 2 (frame-monitor-attribute 'geometry frame x y))
     (/ (car (frame-monitor-attribute 'mm-size frame x y)) 25.4)))


(defun frame-monitor-font-size (&optional frame x y)
  "Return a font size of FRAME's monitor dpi relative to the
`font-size-reference'. The size font returned is never less than
`font-size-minimum'.

If FRAME is omitted or nil, use currently selected frame.

By default, the current monitor is said to be the physical
monitor dominating the selected frame. A frame is dominated by a
physical monitor when either the largest area of the frame
resides in the monitor, or the monitor is the closest to the
frame if the frame does not intersect any physical monitors.

If X and Y are both numbers, then ignore the value of FRAME; the
monitor is determined to be the physical monitor that contains
the pixel coordinate (X, Y)."
  (let* ((font-dpi-ref (car font-size-reference))
         (font-size-ref (cdr font-size-reference))
         (font-size (* font-size-ref (sqrt (/ (frame-monitor-dpi frame x y) font-dpi-ref))))
         (font-size-round (if (< font-size font-size-ref)
                              (ceiling font-size)
                            (floor font-size))))
    (max font-size-minimum font-size-round)))


(defun set-preferred-frame-monitor-font (&optional frame x y)
"Set the first font from `preferred-font-list' that is available
 in FRAME using a font size of FRAME's monitor dpi relative to
 the `font-size-reference'.

If FRAME is t, set font in all existing frames. If FRAME is
omitted or nil, use currently selected frame.

By default, the current monitor is said to be the physical
monitor dominating the selected frame. A frame is dominated by a
physical monitor when either the largest area of the frame
resides in the monitor, or the monitor is the closest to the
frame if the frame does not intersect any physical monitors.

If X and Y are both numbers, then ignore the value of FRAME; the
monitor is determined to be the physical monitor that contains
the pixel coordinate (X, Y).

See `frame-monitor-font-size'."
  (interactive)
  (when-let ((font-name (seq-find (lambda (font) (find-font (font-spec :name font)))
                                  preferred-font-list))
             (font-size (if (framep frame)
                            (frame-monitor-font-size frame x y)
                          (frame-monitor-font-size))))
    (set-frame-font (concat font-name " " (number-to-string font-size)) t
                    (if (framep frame) (list frame) frame))
    (message "Setting font to %s-%d" font-name font-size)))


(defun set-frame-font-scale (&optional arg)
  "Change the default font size of the current frame.

With no prefix argument, increase the font size.

With \\[universal-argument] as prefix argument, descrease the font size.

With \\[universal-argument] \\[universal-argument] as prefix argument, reset the font size to `frame-monitor-font-size'."
  (interactive "P")
  (let ((font-size (if (and (consp arg) (> (prefix-numeric-value arg) 4))
                         (frame-monitor-font-size)
                       (funcall (if (consp arg) '1- '1+) (round (/ (face-attribute 'default :height) 10.0))))))
    (set-face-attribute 'default (window-frame)  :height (* 10 font-size))
    (message "Setting font size to %d" font-size)))

;; key bidings
(global-set-key (kbd "C-M-=") 'set-frame-font-scale)

(provide 'dynamic-font)
