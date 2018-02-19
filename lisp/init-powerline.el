(when (require 'powerline nil t)
  (setq powerline-default-separator 'slant)
  (setq powerline-display-hud nil)
  (setq powerline-gui-use-vcs-glyph t)
  (setq powerline-height 28)


  (defpowerline powerline-modified
    (if buffer-read-only
        (char-to-string #x2717)
      (if (buffer-modified-p)
          (char-to-string #x2733) " ")))

  (defpowerline powerline-mule-info
    (concat
     (when current-input-method
       (concat (unless (eq system-type 'windows-nt)
                 (char-to-string #x1F5AE))
               current-input-method-title " "))
     (prin1-to-string buffer-file-coding-system)))

  (defpowerline powerline-which-func
    (let ((which-func (which-function)))
      (when which-func
        (propertize (concat (char-to-string #x03BB) " " which-func) 'face 'which-func))))

  (defpowerline powerline-remote
    (when (file-remote-p default-directory)
      (concat (if (eq system-type 'windows-nt)
                  "@"
                (char-to-string #x1F4BB))
              tramp-current-host)))

  ;; Modified from the original to handle a different char in Windows
  (defpowerline powerline-vc-custom
    (when (and (buffer-file-name (current-buffer)) vc-mode)
      (if (and window-system (not powerline-gui-use-vcs-glyph))
          (format-mode-line '(vc-mode vc-mode))
        (format " %s%s"
                (if (eq system-type 'windows-nt)
                    (char-to-string #x2191)
                  (char-to-string #xe0a0))
                (format-mode-line '(vc-mode vc-mode))))))

  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-modified mode-line 'l)
                                     (powerline-remote mode-line 'l)
                                     (powerline-buffer-id mode-line-buffer-id 'l)
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc-custom face2 'r)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-which-func face2 'l))
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (when powerline-display-mule-info
                                       (powerline-mule-info face2 'r))
                                     (funcall separator-right face2 face1)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face1 'l))
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (when powerline-display-buffer-size
                                       (concat
                                        (powerline-buffer-size mode-line 'r)
                                        (powerline-raw "|" mode-line 'r)))
                                     (powerline-raw "%6p" mode-line 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face2 face1)))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs))))))

  ;; (powerline-default-theme)
  )
