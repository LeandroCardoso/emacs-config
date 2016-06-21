;; `make-pointer-invisible' is bugged in emacs 24.5.1 on Windows. I am using `mouse-avoidance-mode'
;; as a workaround until the bug be fixed.
(when (eq system-type 'windows-nt)
  (mouse-avoidance-mode 'banish)
  (setq mouse-avoidance-banish-position
   '((frame-or-window . frame)
     (side . right)
     (side-pos . 0)
     (top-or-bottom . top)
     (top-or-bottom-pos . 0))))
