(with-eval-after-load "ibuffer"

  (require 'project)
  (defun ibuffer-set-filter-groups-by-project-root ()
    "Set the current filter groups to filter by project root directory."
    (interactive)
    (setq ibuffer-filter-groups
          (mapcar (lambda (pr)
                    (cons (format "%s" pr) `((filename . ,pr))))
                  (delq nil (ibuffer-remove-duplicates
                          (mapcar (lambda (buf)
                                    (let ((pr (with-current-buffer buf (project-current))))
                                      (when pr
                                        (car (project-roots pr)))))
                                  (buffer-list))))))
    (ibuffer-update nil t))

  ;;(setq ibuffer-display-summary nil)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 40 40 :left :elide)
                " "
                (size 6 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 40 -1) " " filename)))

  (add-hook 'ibuffer-hook #'ibuffer-set-filter-groups-by-project-root)
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode))

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; default is list-buffers
(global-set-key (kbd "C-x 4 C-b") 'ibuffer-other-window)
