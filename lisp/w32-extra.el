(when (eq system-type 'windows-nt)
  (require 'woman)
  (require 'info)

  (defun w32-convert-filename (file-name)
    "Converted slash characters in file names into backslashes."
    (let ((file-name (convert-standard-filename (expand-file-name file-name)))
          (start 0))
      (while (string-match "/" file-name start)
        (aset file-name (match-beginning 0) ?\\)
        (setq start (match-end 0)))
      file-name))

  (defun w32-add-to-path (new-path)
    "Add NEW-PATH to the environment variable \"PATH\""
    (let ((current-path (getenv "PATH"))
          (w32-new-path (w32-convert-filename new-path)))
      (unless (string-match-p (regexp-quote w32-new-path) (or current-path ""))
        (setenv "PATH" (concat w32-new-path path-separator current-path)))))

  (defun w32-add-unix-root-path (path)
    "Set emacs to use an additional custom unix root path."
    (when (file-directory-p path)
      (dolist (bin-dir '("/usr/bin" "/bin"))
        (when (file-directory-p (concat path bin-dir))
          (w32-add-to-path (concat path bin-dir))
          (add-to-list 'exec-path (concat path bin-dir))))
      (dolist (man-dir '("/usr/share/man" "/share/man" "/usr/local/man" "/local/man"))
        (when (file-directory-p (concat path man-dir))
          (add-to-list 'woman-manpath (concat path man-dir))))
      (dolist (info-dir '("/usr/share/info" "/share/info" "/usr/local/info" "/local/info"))
        (when (file-directory-p (concat path info-dir))
          (add-to-list 'Info-additional-directory-list (concat path info-dir))))))

  (defun with-bash-shell (func &rest args)
    "Execute the function FUNC with args ARGS using bash as the default shell.

This function fixes the annoying \"^M\" character that can be
displayed at end of lines in some shell buffer outputs and it can
be used as an advising function for existing functions.

Advising existing function usage:
  (advice-add symbol :around #\\='with-bash-shell)

See `advice-add'."
    (let ((shell-file-name (executable-find "bash"))
          (explicit-bash-args nil)
          (explicit-bash.exe-args nil))
      (apply func args)))

  (defun shell-bash ()
    "Run `shell' with bash"
    (interactive)
    (let ((explicit-shell-file-name (executable-find "bash")))
      (setenv "EMACS" "t")
      (call-interactively 'shell))))

(provide 'w32-extra)
