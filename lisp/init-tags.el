(require 'project)

(defun create-tags (directory)
  "Create a TAGS file at the given DIRECTORY."
  (interactive "DRoot directory: ")
  ; we need a temp buffer because to preserve the current directory buffer
  (with-temp-buffer
    (cd-absolute directory)
    (message "Creating TAGS file at %s" directory)
    (call-process "ctags" nil "*TAGS*" nil "-e" "-R" "--extra=+q" "--fields=+aiS" "--c++-kinds=+p" "-V")))


(defun create-project-tags ()
  "Create a TAGS file at the current root project directory."
  (interactive)
  (mapc 'create-tags (project-roots (project-current t))))


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
