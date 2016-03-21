;; Start the emacs server needed by the emacsclient
(require 'server)
(unless (server-running-p)
  (server-start)
  (message "Server started"))
