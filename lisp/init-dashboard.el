(use-package dashboard
  :ensure t
  :init
  (setopt dashboard-icon-type (if (eq system-type 'windows-nt)
                                  nil
                                'nerd-icons))
  :config
  (require 'dashboard-desktop)
  (setopt dashboard-items '((desktop . 5)
                            (recents . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5)))
  (setopt dashboard-set-file-icons t)
  (setopt dashboard-set-heading-icons t)
  (setopt dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook)
  :hook (dashboard-after-initialize . dashboard-jump-to-desktop))
