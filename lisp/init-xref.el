(with-eval-after-load "xref"
  ;; settings
  (setq xref-search-program (if (executable-find "rg")
                                'ripgrep
                              'grep))
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read)

  ;; save the xref id/pattern in kill-ring
  (defun xref-save-id-advice (input &rest args)
    "Save the xref input into the kill-ring."
    (kill-new input))

  (advice-add 'xref--find-xrefs :after #'xref-save-id-advice)
  (advice-add 'xref-matches-in-files :after #'xref-save-id-advice) ; used by dired and project

  ;; Use bash shell when calling grep/ripgrep, because it fixes the annoying "^M" that can be
  ;; displayed at end of lines.
  (when (eq system-type 'windows-nt)
    (advice-add 'xref-matches-in-files :around #'with-bash-shell)
    (advice-add 'xref-matches-in-directory :around #'with-bash-shell))

  ;; keys
  (define-key xref--xref-buffer-mode-map (kbd "<tab>") 'xref-next-line)
  (define-key xref--xref-buffer-mode-map (kbd "<backtab>") 'xref-prev-line)
  (define-key xref--xref-buffer-mode-map (kbd "M-<return>") 'xref-quit-and-goto-xref))
