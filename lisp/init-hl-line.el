(defun disable-global-hl-line-mode ()
  (setq-local global-hl-line-mode nil))

(global-hl-line-mode)

;; global-hl-line-mode causes slowness when scrolling down repeatedly, this is a good workaround for
;; it.
(setq auto-window-vscroll nil)
