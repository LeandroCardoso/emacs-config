(ido-mode t)
(ido-everywhere t)

(when (require 'ido-ubiquitous nil t)
  (ido-ubiquitous-mode t))

(when (require 'flx-ido nil t)
  ;; disable ido faces to see flx highlights.
  ;; (setq ido-use-faces nil)
  (flx-ido-mode t))

(setq ido-auto-merge-work-directories-length -1)
(setq ido-create-new-buffer 'always)
(setq ido-decorations
      '(" { " " }" " | " " | +" "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
(setq ido-default-buffer-method 'selected-window)
(setq ido-enable-flex-matching t)
(setq ido-rotate-file-list-default t)
(setq ido-show-dot-for-dired t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point t)
(setq ido-use-virtual-buffers 'auto)

;; Enable ido in dired commands
(put 'dired-do-copy   'ido nil)
(put 'dired-do-rename 'ido nil)
