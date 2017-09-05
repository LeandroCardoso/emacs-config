(require 'midnight)

(setq clean-buffer-list-delay-general 2)
(mapc '(lambda (ARG) (add-to-list 'clean-buffer-list-kill-regexps ARG)) '("^\\*.*\\*$"))

(midnight-mode t)
