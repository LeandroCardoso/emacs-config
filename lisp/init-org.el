(with-eval-after-load "org"
  (setq org-M-RET-may-split-line '((default . nil))) ; don't split the line at the cursor position when ALT+ENTER
  (setq org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil)))  ; don't automatically put new line chars
  (setq org-ellipsis 'org-ellipsis)                  ; print ellipsis '...' with custom face
  (setq org-hierarchical-todo-statistics nil)        ; all entries in the subtree are considered.
  (setq org-imenu-depth 3)                           ; maximum level for Imenu access
  (setq org-outline-path-complete-in-steps nil)      ; I don't need this because I have ido-mode
  (setq org-special-ctrl-a/e t)                      ; special headline handling
  (setq org-src-window-setup 'current-window)        ; show edit buffer in the current window
  (setq org-startup-truncated nil)                   ; don't set `truncate-lines', this break long tables
  (setq org-tag-faces `(("doubt" . ,(face-foreground 'warning))
                        ("important" . ,(face-foreground 'org-warning))))
  (setq org-tag-persistent-alist '(("doubt" . ?d) ("important" . ?i)))
  (setq org-tags-column (- fill-column))             ; align tags at the right margin. See `set-org-tags-right-column'
  (setq org-tags-sort-function 'string<)             ; align tags using alphabetic order
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keyword-faces `(("WAITING" . (:foreground ,(face-foreground 'warning) :weight bold))))

  (define-key org-mode-map (kbd "C-c M-t") 'org-toggle-link-display)
  (define-key org-mode-map (kbd "M-<return>") 'org-meta-return) ; workaround to avoid override by a global key

  ;; `set-fill-column' is advised to also set org-tags-column
  (defun set-org-tags-right-column (ARG)
    "Set `org-tags-column' to the negative value of the specified argument."
    (setq org-tags-column (- ARG)))
  (advice-add 'set-fill-column :after #'set-org-tags-right-column)

  ;; org-export (ox)
  (setq org-export-copy-to-kill-ring 'if-interactive)
  (setq org-export-initial-scope 'subtree)
  (setq org-export-preserve-breaks t)
  (setq org-export-with-author nil)
  (setq org-export-with-sub-superscripts nil)
  (setq org-export-with-title nil)
  (setq org-export-with-toc nil)

  ;; ox-ascii
  (setq org-ascii-caption-above t)
  (setq org-ascii-text-width most-positive-fixnum)

  ;; org-mode outside org-mode
  (defvar org-out-keymap nil "Keymap for org-mode commands outside org-mode")
  (setq org-out-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map "l" 'org-store-link)
          (define-key map "t" 'orgtbl-mode)
          (define-key map "s" 'orgalist-mode)
          map))
  (defalias 'org-out-keymap org-out-keymap)
  (global-set-key (kbd "C-c o") 'org-out-keymap))
