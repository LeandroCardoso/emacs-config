(defun set-dbd-environment ()
  "Set my DBD environment"
  (interactive)

  ;; proxy
  (setq url-proxy-services '(("http" . "localhost:3128")
                             ("https" . "localhost:3128")))

  
  ;; Copy binaries to the pen drive after a successful compilation
  (add-hook 'compilation-finish-functions
            (lambda (BUFFER STATUS)
              ;; compilation-mode has some derived modes, like grep-mode, that fire this hook too,
              ;; we should avoid it and only proceed if it is a real compilation.
              (when (eq (buffer-local-value 'major-mode BUFFER) 'compilation-mode)
                (hack-dir-local-variables-non-file-buffer)
                (msvs-copy-bin-to-drive))))
  )


(when (equal "DIEBOLD_MASTER" (getenv "USERDOMAIN"))
  (set-dbd-environment))
