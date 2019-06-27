;; Start the emacs server needed by the emacsclient
(require 'server)
(unless (and noninteractive (eq (server-running-p) t))
  (server-start)
  (message "Server started"))
