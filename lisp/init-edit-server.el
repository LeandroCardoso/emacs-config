(when (require 'edit-server nil t)
  (setq edit-server-default-major-mode 'org-mode)
  (setq edit-server-new-frame-alist
        '((name . "Edit Server")
          (width . 0.5)
          (height . 0.5)
          (fullscreen . nil)))
  (edit-server-start))
