(winner-mode)

(define-key winner-mode-map (kbd "C-c [") 'winner-undo)
(define-key winner-mode-map (kbd "C-c ]") 'winner-redo)

(with-eval-after-load "which-key"
  (add-to-list 'winner-boring-buffers which-key-buffer-name))
