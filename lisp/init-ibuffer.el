;;(setq ibuffer-display-summary nil)
(setq ibuffer-formats
   '((mark modified read-only " "
           (name 40 40 :left :elide)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " " (name 40 -1) " " filename)))

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode)))

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; default is list-buffers
(global-set-key (kbd "C-x 4 C-b") 'ibuffer-other-window)
