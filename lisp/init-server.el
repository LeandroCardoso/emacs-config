;; Start the emacs server needed by the emacsclient
(require 'server)
(unless (eq (server-running-p) t)
  (server-start)
  (message "Server started"))
