;;; dashboard-desktop.el --- Integrate desktop with dashboard -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;; A 'desktop' item must be manually added to the 'dashboard-items variable:
;;
;; (add-to-list 'dashboard-items '(desktop . 5))

;;; Code:

(require 'dashboard)
(require 'desktop)

(defun dashboard-insert-desktop (list-size)
    "Add a list of LIST-SIZE items of desktop files to load."
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

;; FIXME
;; (add-to-list 'dashboard-heading-icons
;;              (pcase dashboard-icon-type
;;                ('all-the-icons '(desktop . "desktop"))
;;                ('nerd-icons '(desktop . "nf-oct-desktop"))))
(add-to-list 'dashboard-item-generators '(desktop . dashboard-insert-desktop))
(add-to-list 'dashboard-item-shortcuts '(desktop . "d"))

(provide 'dashboard-desktop)

;;; dashboard-desktop.el ends here
