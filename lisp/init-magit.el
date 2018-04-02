(with-eval-after-load "magit"
  ;; settings
  (setq magit-completing-read-function 'magit-ido-completing-read)

  ;; Windows specific settings
  (when (eq system-type 'windows-nt)
    (setq w32-pipe-read-delay 0)
    (setq magit-process-connection-type nil)
    (setq magit-refresh-status-buffer nil))

  ;; key bindings
  (define-key magit-mode-map [remap previous-line] 'magit-previous-line)
  (define-key magit-mode-map [remap next-line] 'magit-next-line)

  (define-key magit-file-section-map (kbd "SPC") 'magit-diff-visit-file-other-window)
  (define-key magit-hunk-section-map (kbd "SPC") 'magit-diff-visit-file-other-window))

;; global keymap
(defvar magit-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'magit-dispatch-popup)
    (define-key map "f" 'magit-file-popup)
    (define-key map "s" 'magit-status)
    (define-key map "i" 'magit-init)
    (define-key map "c" 'magit-clone)
    (define-key map "d" 'magit-diff-buffer-file)
    (define-key map "D" 'magit-diff-buffer-file-popup)
    (define-key map "l" 'magit-log-buffer-file)
    (define-key map "L" 'magit-log-buffer-file-popup)
    (define-key map "b" 'magit-blame)
    (define-key map "B" 'magit-blame-popup)
    map)
  "Keymap for global magit commands")

(defalias 'magit-global-keymap magit-global-keymap)
(global-set-key (kbd "C-x g") 'magit-global-keymap)
