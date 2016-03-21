;; Start the emacs server needed by the emacsclient
(if (require 'server nil t)
  (unless (server-running-p)
    (server-start)
    (message "Server started")))
