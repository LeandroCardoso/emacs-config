;;; dashboard-desktop.el --- Integrate desktop.el with dashboard.el -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;; To use this package a 'desktop' item must be manually added to the `dashboard-items' variable:
;;
;; (add-to-list 'dashboard-items '(desktop . 5))

;; Note: loading this package causes the 'desktop' package to be loaded, which may cause a desktop
;; session to be automatically loaded by the 'desktop' package when the `desktop-save-mode' is
;; enabled.  If this behavior is undesirable, my recommendation is to only enable the
;; `desktop-save-mode' after a desktop session is loaded:
;;
;; (add-hook 'desktop-after-read-hook 'desktop-save-mode)

;;; Code:

(require 'dashboard)
(require 'desktop)

(defvar dashboard-desktop-alist nil
  "Alist records shorten's desktop files and it's full paths.")

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

;; FIXME face is not been applied
(defun dashboard-desktop-insert-heading-advice (args)
  "Advice function to insert the desktop icon in the heading.

Argument ARGS is the same as the `dashboard-insert-heading' : (heading
shortcut icon)."
  (if (equal (car args) "Desktop:")
      ;; Add the desktop icon defined in `dashboard-heading-icons' as the third parameter to the
      ;; `dashboard-insert-heading'.
      (list (nth 0 args)
            (nth 1 args)
            (dashboard-octicon (cdr (assoc 'desktop dashboard-heading-icons))
                               `( :height   ,dashboard-heading-icon-height
                                  :v-adjust ,dashboard-heading-icon-v-adjust
                                  :face     dashboard-heading)))
    args))

(advice-add 'dashboard-insert-heading :filter-args 'dashboard-desktop-insert-heading-advice)

(add-to-list 'dashboard-heading-icons
             (pcase dashboard-icon-type
               ('all-the-icons '(desktop . "device_desktop"))
               ('nerd-icons '(desktop . "nf-oct-device_desktop"))))
(add-to-list 'dashboard-item-generators '(desktop . dashboard-insert-desktop))
(add-to-list 'dashboard-item-shortcuts '(desktop . "d"))

(provide 'dashboard-desktop)

;;; dashboard-desktop.el ends here
