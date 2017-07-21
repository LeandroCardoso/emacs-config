(setq ediff-custom-diff-options "-c -w")
(setq ediff-diff-options "--binary -w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; everything in one frame

;; I don't know why I need such a complex code to set the keymap with ediff-mult, but this works.
(with-eval-after-load "ediff-mult"
  (add-hook 'ediff-meta-buffer-keymap-setup-hook
            (lambda ()
              (define-key ediff-meta-buffer-map (kbd "<tab>") 'ediff-next-meta-item)
              (define-key ediff-meta-buffer-map (kbd "<backtab>") 'ediff-previous-meta-item))))

;; global keymap
(defvar ediff-global-keymap
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
    (define-key map "m" 'ediff-show-registry)
    (define-key-map "r" 'ediff-regions-linewise)
    (define-key-map "R" 'ediff-regions-wordwise)
    map)
  "Keymap for global ediff commands")

(defalias 'ediff-global-keymap ediff-global-keymap)
(global-set-key (kbd "C-x M-e") 'ediff-global-keymap)
