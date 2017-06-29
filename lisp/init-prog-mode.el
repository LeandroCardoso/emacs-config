(with-eval-after-load 'prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\)\\>" 1 font-lock-warning-face t))))))
