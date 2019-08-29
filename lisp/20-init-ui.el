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
  (other-window (if (numberp count) (- count) count) t)
  (select-frame-set-input-focus (selected-frame)))


(defun other-window-or-frame (arg)
  "`other-frame', if `one-window-p'; otherwise, `other-window'."
  (interactive "p")
  (if (one-window-p) (other-frame arg) (other-window arg)))


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

With parameter ARG, call `toggle-frame-fullscreen' for all visible frames.

Without parameter ARG, behavior like `toggle-frame-fullscreen'. "
  (interactive "P")
  (if arg
      (dolist (frame (frame-list))
        (when (frame-visible-p frame)
          (with-selected-frame frame
            (toggle-frame-fullscreen))))
    (toggle-frame-fullscreen)))


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


;; No need to waste precious desktop space with useless GUI
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; cursor
(setq-default cursor-type 'bar)


;; Font
(let ((font (cond
             ((eq system-type 'gnu/linux) '("Source Code Pro" . "11"))
             ((eq system-type 'windows-nt) '("Consolas" . "10")))))
  (if (member (car font) (font-family-list))
      (set-frame-font (concat (car font) " " (cdr font)) t t)
    (message "Warning: Font %s does not exist" (car font))))

(setq text-scale-mode-step 1.1)


;; Modeline
(setq column-number-mode t)
(setq mode-line-default-help-echo nil)
(set-face-attribute 'mode-line-highlight nil :box 'unspecified)


;; Fringe
(setq-default indicate-empty-lines t)
(setq next-error-highlight 'fringe-arrow)


;; Window
(setq split-height-threshold nil)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-horizontally)

;; reuse frames with `display-buffer-other-frame'
(setcar display-buffer--other-frame-action
     '(display-buffer-reuse-window display-buffer-use-some-frame display-buffer-pop-up-frame))

(dolist (buf '("^\\*Flycheck"
               "^\\*Occur\\*"
               "^\\*Woman"
               "^\\*grep\\*"
               "^\\*xref\\*"
               "^\\*company"))
  (add-to-list 'display-buffer-alist
               `(,buf . ((display-buffer-pop-up-window) . ((window-height . 0.3)))) t))

(dolist (buf '("^\\*Apropos\\*"
               "^\\*Help\\*"
               "^\\* OmniSharp"
               "^\\*compilation\\*"
               "^\\*info\\*"))
  (add-to-list 'display-buffer-alist
               `(,buf . ((display-buffer-reuse-window) . ((window-height . 0.3)))) t))

;; Frame
(setq frame-title-format (concat "%b - emacs@" (system-name)))
(setq frame-inhibit-implied-resize t) ;; never resize the frame
(setq initial-frame-alist '((fullscreen . maximized) (vertical-scroll-bars . nil)))
(setq default-frame-alist initial-frame-alist)
(setq window-system-default-frame-alist '((x . ((alpha . 95)))))
;; workaround to set cursor color in new frames
(add-hook 'after-make-frame-functions
          (lambda(FRAME)
            (modify-frame-parameters FRAME `((cursor-color . ,(face-background 'cursor))))))


;; key-bindings
(global-set-key (kbd "M-o") 'other-window-all-frames)
(global-set-key (kbd "M-O") 'other-window-all-frames-backward)
(global-set-key (kbd "C-x o") 'other-window-or-frame) ;; default is other-window
(global-set-key (kbd "C-x M-o") 'other-frame)

(global-set-key (kbd "C-x 4 k") 'kill-other-buffer-and-window)
(global-set-key (kbd "C-x M-t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x c") 'clone-buffer)

(global-set-key [remap toggle-frame-fullscreen] 'toggle-frame-fullscreen+)
