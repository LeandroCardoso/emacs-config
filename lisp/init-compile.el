(setq compilation-scroll-output 'first-error)
(setq compilation-error-screen-columns nil)
;; TODO require project
(setq compilation-save-buffers-predicate (lambda ()
                                           (let ((project-root (project-root)))
                                             (when project-root
                                               (string-prefix-p project-root (file-truename (buffer-file-name)))))))
