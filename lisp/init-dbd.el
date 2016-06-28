(defun set-dbd-environment ()
  "Set my DBD environment"
  (interactive)
  ;; proxy
  (setq url-proxy-services '(("http" . "localhost:3128")
                             ("https" . "localhost:3128")))

  ;; Copy binaries to the pen drive after a successful compilation
  (add-hook 'compilation-finish-functions
            (lambda (BUFFER STATUS)
              (hack-dir-local-variables-non-file-buffer)
              (msvs-copy-bin-to-drive)))
  )


(when (equal "DIEBOLD_MASTER" (getenv "USERDOMAIN"))
  (set-dbd-environment))
