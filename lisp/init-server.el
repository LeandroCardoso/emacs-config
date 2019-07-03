;; Start the emacs server needed by the emacsclient
(require 'server)
(unless (or noninteractive (eq (server-running-p) t))
  (server-start)
  (message "Server started"))
