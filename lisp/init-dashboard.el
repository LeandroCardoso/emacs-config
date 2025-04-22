(use-package dashboard
  :ensure t
  :init
  (setopt dashboard-icon-type 'nerd-icons)
  :config
  (defun dashboard-insert-desktop (list-size)
    "Add a list of LIST-SIZE items of desktop files to load."
    (require 'desktop)
    (dashboard-insert-section
     "Desktop:"
     (dashboard-shorten-paths
      (dashboard-subseq (remq nil
                             (mapcar #'(lambda (path)
                                         (if (file-exists-p (concat path desktop-base-file-name))
                                             path))
                                     desktop-path))
                        list-size)
      'dashboard-desktop-alist 'desktop)
     list-size
     'desktop
     (dashboard-get-shortcut 'desktop)
     `(lambda (&rest _)
        (desktop-read (dashboard-expand-path-alist ,el dashboard-desktop-alist)))
     (format "%s" (dashboard-expand-path-alist el dashboard-desktop-alist))))

  (add-to-list 'dashboard-item-generators '(desktop . dashboard-insert-desktop))
  (add-to-list 'dashboard-item-shortcuts '(desktop . "d"))

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
