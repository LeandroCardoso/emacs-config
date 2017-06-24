(defun set-frame-title-from-desktop-dir ()
  (setq frame-title-format
        (list "emacs"
              (when (boundp 'desktop-dirname)
                (list " - " (file-name-nondirectory (directory-file-name desktop-dirname)))))))

(with-eval-after-load "desktop"
  (add-to-list 'desktop-path ".")
  (setq desktop-save 'ask-if-exists)

  (desktop-save-mode)

  (add-hook 'desktop-after-read-hook 'set-frame-title-from-desktop-dir)
  (add-hook 'desktop-save-hook 'set-frame-title-from-desktop-dir))
