;; functions

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
         ;; If WINDOW is the only window on its frame and is not the
         ;; minibuffer window, try to split it vertically disregarding
         ;; the value of `split-height-threshold'.
         (let ((split-height-threshold 0))
           (when (window-splittable-p window)
         (with-selected-window window
           (split-window-below))))))))


(defun other-window-backward (count &optional all-frames)
  "A backward version of `other-window'."
  (interactive "p")
  (other-window (if (numberp count) (- count) count) all-frames))


(defun other-window-all-frames (count)
  "Select another window in all frames in cyclic ordering of
windows and frames. COUNT specifies the number of windows to
skip, starting with the selected window, before making the
selection. If COUNT is positive, skip COUNT windows forwards. If
COUNT is negative, skip -COUNT windows backwards. COUNT zero
means do not skip any window, so select the selected window. In
an interactive call, COUNT is the numeric prefix argument. Return
nil.

This function uses ‘other-window’ with argument ALL-FRAMES=t for
finding the window to select."
  (interactive "p")
  (other-window count t)
  (select-frame-set-input-focus (selected-frame)))


(defun other-window-all-frames-backward (count)
  "Select another window in all frames in cyclic ordering of
windows and frames. COUNT specifies the number of windows to
skip, starting with the selected window, before making the
selection. If COUNT is positive, skip COUNT windows forwards. If
COUNT is negative, skip -COUNT windows backwards. COUNT zero
means do not skip any window, so select the selected window. In
an interactive call, COUNT is the numeric prefix argument. Return
nil.

This function uses ‘other-window’ with argument ALL-FRAMES=t for
finding the window to select."
  (interactive "p")
  (other-window-backward count t)
  (select-frame-set-input-focus (selected-frame)))


(defun resize-window-to-region (start end)
  "Resize current window vertically to fit the size of the active region"
  (interactive "r")
  (when mark-active
    (window-resize nil (1+ (- (count-screen-lines start end) (window-body-height))))
    (recenter (count-lines start (point)))))


(defun kill-other-buffer-and-window (count)
  "Kill other buffer.
See `kill-buffer' and `other-window'"
  (interactive "p")
  (save-selected-window
    (let ((curbuf (current-buffer)))
      (other-window count)
      (when (not (eq curbuf (current-buffer)))
        (kill-buffer-and-window)))))


(defun toggle-frame-fullscreen+ (arg)
  "Toggle fullscreen state of selected frame.
Make selected frame fullscreen or restore its previous size if it
is already fullscreen.

With parameter ARG, toggle selected frame state and toggle all
the other frames fullscreen state when their state is different
from the selected frame.

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


(defun maximize-frame-unless-fullscreen ()
  (unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
    (set-frame-parameter nil 'fullscreen 'maximized)))


;; From obsolete lucid.el
(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.
With a numeric arg N, switch to the Nth most recent buffer.
With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
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


(defun list-fonts ()
  "Display a buffer with a list of available fonts."
  (interactive)
  (with-current-buffer-window "*fonts*" nil nil
    (dolist (font (x-list-fonts "*"))
      (insert (format "%s\n" font)))))


;; No need to waste precious desktop space with useless GUI
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; cursor
(setq-default cursor-type 'bar)


;; Font
(setq inhibit-compacting-font-caches t)

(defcustom preferred-font-list '("Source Code Pro" "Cascadia Mono" "Consolas")
  "A list of preferred fonts."
  :type '(repeat string))

(defun set-preferred-font ()
  "Set the first font from `preferred-font-list' that is available
in all frames."
  (interactive)
  (when-let ((font-name (seq-find (lambda (font) (find-font (font-spec :name font)))
                                  preferred-font-list)))
    (set-frame-font font-name t t)
    (message "Setting font to %s" font-name)))

(set-preferred-font)

(setq text-scale-mode-step 1.1)


;; Better underline
(setq x-underline-at-descent-line t)


;; Modeline
(setq column-number-mode t)
(setq mode-line-default-help-echo nil)
(setq mode-line-position-column-line-format '(" %l:%c"))
(set-face-attribute 'mode-line-highlight nil :box 'unspecified)


;; Fringe
(setq-default indicate-empty-lines t)
(setq next-error-highlight 'fringe-arrow)


;; Window
(setq split-height-threshold nil)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-horizontally)

;; Frame
(setq frame-inhibit-implied-resize t) ;; never resize the frame
(setq initial-frame-alist '((fullscreen . maximized) (vertical-scroll-bars . nil)))
(setq default-frame-alist initial-frame-alist)
(setq window-system-default-frame-alist '((x . ((alpha . 96)))
                                          (pgtk . ((alpha . 96)))))

;; key-bindings

;; The "all-frames" version of other-window are confusing when using virtual desktops in linux, so
;; just set it in Windows when there is more than one monitor.
(if (and (eq system-type 'windows-nt)
         (> 1 (length (display-monitor-attributes-list))))
    (progn
      (global-set-key (kbd "M-o") 'other-window-all-frames)
      (global-set-key (kbd "M-O") 'other-window-all-frames-backward))
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-O") 'other-window-backward))

(define-key ctl-x-map (kbd "o") 'other-frame) ; original is other-window
(define-key ctl-x-4-map (kbd "k") 'kill-other-buffer-and-window)
(global-set-key [remap toggle-frame-fullscreen] 'toggle-frame-fullscreen+)
