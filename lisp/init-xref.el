(with-eval-after-load "xref"
  ;; settings
  (setq xref-search-program (if (executable-find "rg")
                                'ripgrep
                              'grep))
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read)

  ;; save the xref id/pattern in kill-ring
  (defun xref-save-id-advice (id &optional arg1 arg2 arg3)
    (kill-new id))

  (advice-add 'xref--find-definitions :after #'xref-save-id-advice)
  (advice-add 'xref--find-xrefs :after #'xref-save-id-advice)

  ;; keys
  (define-key xref--xref-buffer-mode-map (kbd "<tab>") 'xref-next-line)
  (define-key xref--xref-buffer-mode-map (kbd "<backtab>") 'xref-prev-line)
  (define-key xref--xref-buffer-mode-map (kbd "M-<return>") 'xref-quit-and-goto-xref)

  (define-key xref--xref-buffer-mode-map (kbd "u") 'rename-uniquely)
  (define-key xref--xref-buffer-mode-map (kbd "k") 'keep-lines)
  (define-key xref--xref-buffer-mode-map (kbd "f") 'flush-lines))
