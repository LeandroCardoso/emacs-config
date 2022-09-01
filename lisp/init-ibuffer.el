(with-eval-after-load "ibuffer"
  ;; Modified version of ibuffer column "name" with the uniquify part striped
  (define-ibuffer-column base-name
    (:name "Name"
           :inline t
           :header-mouse-map ibuffer-name-header-map
           :props
           ('mouse-face 'highlight 'keymap ibuffer-name-map
                        'ibuffer-name-column t
                        'help-echo '(if tooltip-mode
                                        "mouse-1: mark this buffer\nmouse-2: select this buffer\nmouse-3: operate on this buffer"
                                      "mouse-1: mark buffer   mouse-2: select buffer   mouse-3: operate"))
           :summarizer
           (lambda (strings)
             (let ((bufs (length strings)))
               (cond ((zerop bufs) "No buffers")
                     ((= 1 bufs) "1 buffer")
                     (t (format "%s buffers" bufs))))))
    (let* ((name (or (uniquify-buffer-base-name) (buffer-name)))
           (string (propertize name
                               'font-lock-face
                               (ibuffer-buffer-name-face buffer mark))))
      (if (not (seq-position string ?\n))
          string
        (replace-regexp-in-string
         "\n" (propertize "^J" 'font-lock-face 'escape-glyph) string))))

  ;; settings
  (setq ibuffer-display-summary nil)
  (setq ibuffer-marked-char ?*)
  (setq ibuffer-modified-char ?M)
  (setq ibuffer-read-only-char ?R)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (base-name 60 60 :left :elide) " "
                (size 6 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process)
          (mark " " (name 40 -1) " " filename)))

  ;; Remove underline from title
  (defun ibuffer-title-remove-underline (format)
    (ibuffer-assert-ibuffer-mode)
    (save-excursion
      (goto-line 2)
      (delete-region (point-at-bol) (+ (point-at-eol) 1))))

  (advice-add 'ibuffer-update-title-and-summary :after #'ibuffer-title-remove-underline))

(global-set-key (kbd "C-x C-b") 'ibuffer) ; original is list-buffers
(global-set-key (kbd "C-x 4 C-b") 'ibuffer-other-window)
