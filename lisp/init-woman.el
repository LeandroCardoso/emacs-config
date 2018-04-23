(with-eval-after-load "woman"
  (setq woman-fill-frame t)
  (setq woman-use-symbol-font t)
  (set-face-attribute 'woman-bold nil :inherit '(Man-overstrike))
  (set-face-attribute 'woman-italic nil :inherit '(Man-underline)))

;; unset compose-mail keys to use it with woman
(global-unset-key (kbd "C-x m"))   ;; compose-mail
(global-unset-key (kbd "C-x 4 m")) ;; compose-mail-other-window
(global-unset-key (kbd "C-x 5 m")) ;; compose-mail-other-frame

(global-set-key (kbd "C-x m") 'woman)
