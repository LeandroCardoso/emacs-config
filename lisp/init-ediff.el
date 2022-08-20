(with-eval-after-load "ediff"
  ;; Copy AB to C
  ;; Adapted from https://stackoverflow.com/a/29757750
  (defun ediff-copy-AB-to-C (reverse)
    "Copy current difference region from buffer A and buffer B to buffer C.

With ARG, copy in reverse order - buffer A past buffer B to
buffer C."
    (interactive "P")
    (ediff-barf-if-not-control-buffer)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents
                       ediff-current-difference
                       (if reverse 'B 'A)
                       ediff-control-buffer)
                      (ediff-get-region-contents
                       ediff-current-difference
                       (if reverse 'A 'B)
                       ediff-control-buffer)))
    (ediff-recenter))

  (defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map (kbd "d") 'ediff-copy-AB-to-C))

  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


  ;; Text scale
  (defun ediff-text-scale-increase (inc)
    "docstring"
    (interactive "p")
    (ediff-barf-if-not-control-buffer)
    (dolist (buf `(,ediff-buffer-A ,ediff-buffer-B ,ediff-buffer-C ,ediff-ancestor-buffer))
      (when buf
        (with-current-buffer buf
          (text-scale-increase inc)))))

  (defun ediff-text-scale-decrease (dec)
    "docstring"
    (interactive "p")
    (ediff-text-scale-increase (- dec)))

  (defun ediff-text-scale-reset ()
    "docstring"
    (interactive)
    (ediff-text-scale-increase 0))

  (defun add-text-scale-to-ediff-mode-map ()
    (define-key ediff-mode-map (kbd "C-+") 'ediff-text-scale-increase)
    (define-key ediff-mode-map (kbd "C--") 'ediff-text-scale-decrease)
    (define-key ediff-mode-map (kbd "C-0") 'ediff-text-scale-reset))

  (add-hook 'ediff-keymap-setup-hook 'add-text-scale-to-ediff-mode-map)
  (add-hook 'ediff-cleanup-hook 'ediff-text-scale-reset)
  (add-hook 'ediff-suspend-hook 'ediff-text-scale-reset)


  ;; (setq ediff-custom-diff-options "-c -w")
  ;; (setq ediff-diff-options "--binary -w")
  (setq ediff-show-ancestor nil)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain) ; everything in one frame
  )

(with-eval-after-load "ediff-mult"
  (add-hook 'ediff-meta-buffer-keymap-setup-hook
            (lambda ()
              (define-key ediff-meta-buffer-map (kbd "<tab>") 'ediff-next-meta-item)
              (define-key ediff-meta-buffer-map (kbd "<backtab>") 'ediff-previous-meta-item))))


;; global keymap
(defvar ediff-keymap nil "Keymap for ediff commands")
(defvar ediff-merge-keymap nil "Keymap for ediff merge commands")
(defvar ediff-patch-keymap nil "Keymap for ediff patch commands")

(setq ediff-merge-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map "b" 'ediff-merge-buffers)
        (define-key map "B" 'ediff-merge-buffers-with-ancestor)
        (define-key map "d" 'ediff-merge-directories)
        (define-key map "D" 'ediff-merge-directories-with-ancestor)
        (define-key map "t" 'ediff-merge-directory-revisions)
        (define-key map "T" 'ediff-merge-directory-revisions-with-ancestor)
        (define-key map "f" 'ediff-merge-files)
        (define-key map "F" 'ediff-merge-files-with-ancestor)
        (define-key map "v" 'ediff-merge-revisions)
        (define-key map "V" 'ediff-merge-revisions-with-ancestor)
        map))
(defalias 'ediff-merge-keymap ediff-merge-keymap)

(setq ediff-patch-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map "b" 'ediff-patch-buffer)
        (define-key map "f" 'ediff-patch-file)
        map))
(defalias 'ediff-patch-keymap ediff-patch-keymap)

(setq ediff-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map "b" 'ediff-buffers)
        (define-key map "B" 'ediff-buffers3)
        (define-key map "c" 'ediff-current-file)
        (define-key map "d" 'ediff-directories)
        (define-key map "D" 'ediff-directories3)
        (define-key map "f" 'ediff-files)
        (define-key map "F" 'ediff-files3)
        (define-key map "h" 'ediff-documentation)
        (define-key map "k" 'ediff-backup)
        (define-key map (kbd "<return>") 'ediff-show-registry)
        (define-key map "l" 'ediff-regions-linewise)
        (define-key map "w" 'ediff-regions-wordwise)
        (define-key map "v" 'ediff-revision)
        (define-key map "t" 'ediff-directory-revisions)
        (define-key map "m" 'ediff-merge-keymap)
        (define-key map "p" 'ediff-patch-keymap)
        map))
(defalias 'ediff-keymap ediff-keymap)
(global-set-key (kbd "C-x M-e") 'ediff-keymap)
