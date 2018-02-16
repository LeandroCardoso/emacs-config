(with-eval-after-load "org"
  (setq org-M-RET-may-split-line '((default . nil))) ; don't split the line at the cursor position when ALT+ENTER
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))) ; don't automatically put new line chars
  (setq org-completion-use-ido t)             ; use ido completion wherever possible
  (setq org-ellipsis 'org-ellipsis)           ; print ellipsis '...' with custom face
  (setq org-hierarchical-todo-statistics nil) ; all entries in the subtree are considered.
  (setq org-imenu-depth 6)                    ; maximum level for Imenu access
  (setq org-outline-path-complete-in-steps nil) ; I don't need this because I have ido-mode
  (setq org-special-ctrl-a/e t)                 ; special headline handling
  (setq org-src-fontify-natively t)             ; fontify code in code blocks
  (setq org-src-tab-acts-natively t)            ; use native major mode TAB in src code block
  (setq org-src-window-setup 'current-window)   ; show edit buffer in the current window
  (setq org-startup-folded nil)                 ; open org files in unfolded mode
  (setq org-startup-truncated nil)              ; don't set `truncate-lines', this break long tables
  (setq org-tags-column (- fill-column)) ; align tags at the right margin. See `set-org-tags-right-column'
  (setq org-tags-sort-function 'string<) ; align tags using alphabetic order

  (define-key org-mode-map (kbd "C-c M-t") 'org-toggle-link-display)

  ;; org-mode outside org-mode
  (defvar org-out-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map "l" 'org-store-link)
      (define-key map "t" 'orgtbl-mode)
      (define-key map "s" 'orgstruct-mode)
      map)
    "Keymap for org-mode commands outside org-mode")
  (defalias 'org-out-keymap org-out-keymap)
  (global-set-key (kbd "C-c o") 'org-out-keymap)


  ;; `set-fill-column' is advised to also set org-tags-column
  (defun set-org-tags-right-column (ARG)
    "Set `org-tags-column' to the negative value of the specified argument."
    (setq org-tags-column (- ARG)))

  (advice-add 'set-fill-column :after #'set-org-tags-right-column)
  )
