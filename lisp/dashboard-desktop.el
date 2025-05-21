;;; dashboard-desktop.el --- Integrate desktop.el with dashboard.el -*- lexical-binding: t; -*-

;; Copyright (C) Leandro Cardoso

;; Maintainer: Leandro Cardoso <leandrocardoso@gmail.com>

;;; Commentary:

;; This package integrates `desktop.el` with `dashboard.el` by adding a new "desktop" item to the
;; dashboard.  To enable it, add the following entry to `dashboard-items`:
;;
;;   (add-to-list 'dashboard-items '(desktop . 5))
;;
;; Note: Loading this package will also load `desktop.el`, which may automatically load a saved
;; session if `desktop-save-mode` is enabled.  To prevent this behavior, consider enabling
;; `desktop-save-mode` only *after* loading a session:
;;
;;   (add-hook 'desktop-after-read-hook #'desktop-save-mode)

;;; Code:

(require 'dashboard)
(require 'desktop)

(defvar dashboard-desktop-alist nil
  "Alist mapping shortened desktop file paths to their full paths.")

(defun dashboard-insert-desktop (list-size)
  "Insert a list of LIST-SIZE desktop sessions into the dashboard."
  (dashboard-insert-section
   "Desktop:"
   (dashboard-shorten-paths
    (dashboard-subseq
     (delq nil
           (mapcar (lambda (path)
                     (when (file-exists-p (expand-file-name desktop-base-file-name path))
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

;; FIXME: Icon face is not being applied correctly.
(defun dashboard-desktop-insert-heading-advice (args)
  "Advice to insert the desktop icon into the heading.

ARGS is a list in the format (heading shortcut icon), passed to
`dashboard-insert-heading`."
  (if (equal (car args) "Desktop:")
      (list (nth 0 args)
            (nth 1 args)
            (dashboard-octicon (cdr (assoc 'desktop dashboard-heading-icons))
                               `( :height   ,dashboard-heading-icon-height
                                  :v-adjust ,dashboard-heading-icon-v-adjust
                                  :face     dashboard-heading)))
    args))

(advice-add 'dashboard-insert-heading :filter-args #'dashboard-desktop-insert-heading-advice)

(add-to-list 'dashboard-heading-icons
             (pcase dashboard-icon-type
               ('all-the-icons '(desktop . "device_desktop"))
               ('nerd-icons     '(desktop . "nf-oct-device_desktop"))))

(add-to-list 'dashboard-item-generators '(desktop . dashboard-insert-desktop))
(add-to-list 'dashboard-item-shortcuts '(desktop . "d"))

(provide 'dashboard-desktop)

;;; dashboard-desktop.el ends here
