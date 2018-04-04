(require 'project)

(defalias 'ctags-call-process
  (apply-partially 'call-process
                   "ctags" nil "*TAGS*" nil
                   "-e" "-R" "-V" "--extra=+q" "--fields=+aiS" "--c++-kinds=+p" "--langmap=c++:+.c"))

(defun create-tags (directory)
  "Create a TAGS file at the given DIRECTORY."
  (interactive "DRoot directory: ")
  ; we need a temp buffer because to preserve the current directory buffer
  (with-temp-buffer
    (cd-absolute directory)
    (message "Creating TAGS file at %s" directory)
    (ctags-call-process)))


(defun create-project-tags ()
  "Create a TAGS file at the current root project directory."
  (interactive)
  (mapc 'create-tags (project-roots (project-current t))))


(defun update-tags-for-current-file ()
  "Update the TAGS file located in a parent directory for the current file."
  (interactive)
  (let* ((file buffer-file-name)
         (directory (locate-dominating-file (or file "") "TAGS")))
    (when directory
      (with-temp-buffer
        (cd-absolute directory)
        (message "Update TAGS file at %s for %s" directory file)
        (ctags-call-process "-a" file)))))


(add-hook 'after-save-hook 'update-tags-for-current-file)

(defadvice find-tag (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window))

(defadvice find-tag-other-window (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window))

(defadvice find-tag-other-frame (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window))

(ad-activate 'find-tag)
(ad-activate 'find-tag-other-window)
(ad-activate 'find-tag-other-frame)

(setq tags-revert-without-query t)
