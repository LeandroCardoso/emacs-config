(require 'info)

(defun add-unix-root-dir (DIRNAME)
  "Set emacs to use an additional custom unix root directory.
Custom directories are added in the begging"
  (when (file-directory-p DIRNAME)
    (progn
      (dolist (DIR '("/usr/bin" "/bin"))
        (when (file-directory-p (concat DIRNAME DIR))
          (setenv "PATH" (concat (convert-standard-filename (concat DIRNAME DIR))
                               path-separator
                               (getenv "PATH")))
          (add-to-list 'exec-path (concat DIRNAME DIR))))
      (dolist (DIR '("/usr/share/man" "/share/man" "/usr/local/man" "/local/man"))
        (when (file-directory-p (concat DIRNAME DIR))
          (add-to-list 'woman-manpath (concat DIRNAME DIR))))
      (dolist (DIR '("/usr/share/info" "/share/info" "/usr/local/info" "/local/info"))
        (when (file-directory-p (concat DIRNAME DIR))
          (add-to-list 'Info-additional-directory-list (concat DIRNAME DIR)))))))


(defun smart-newline-and-indent ()
    "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
    (interactive)
    (move-end-of-line nil)
    (newline-and-indent))


(defun toggle-indent-tabs-mode ()
  "Toggle the value of `indent-tabs-mode'."
  (interactive)
  (message "indent-tabs-mode: %S" (setq indent-tabs-mode (not indent-tabs-mode))))


(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))


(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))


(defun duplicate-line-or-region (&optional arg)
  "Duplicate the active region or current line
With optinal arg, duplicate arg times"
  (interactive "*p")
  (let ((buffer (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (buffer-substring (point-at-bol) (point-at-eol)))))
    (save-excursion
      (end-of-line)
      (dotimes (i arg)
        (insert "\n")
        (insert buffer))))
  ;; line-move-1 keeps the cursor at the original position
  (line-move-1 (or arg 1)))


(defun resize-window-to-region (start end)
  "Resize current window vertically to fit the size of the active region"
  (interactive "r")
  (when mark-active
    (window-resize nil (1+ (- (count-screen-lines start end) (window-body-height))))
    (recenter (count-lines start (point)))))


;; From obsolete lucid.el
(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.
With a numeric arg N, switch to the Nth most recent buffer.
With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth arg
      (apply 'nconc
         (mapcar
          (lambda (buf)
            (if (= ?\  (string-to-char (buffer-name buf)))
            nil
              (list buf)))
          (buffer-list)))))))


(defun smart-dot-comma ()
  "Go to end of line and add a \";\""
  (interactive)
  (move-end-of-line 1)
  (insert ";"))


(defun mktags (DIR)
  "Create a TAGS file at the given directory for c++ files"
  (interactive "DRoot directory: ")
  ; we need a temp buffer because to preserve the current directory buffer
  (with-temp-buffer
    (cd-absolute DIR)
    (message "Creating TAGS file at %s" DIR)
    (call-process "ctags" nil "*TAGS*" nil "-e" "-R" "--extra=+q" "--fields=+aiS" "--c++-kinds=+p" "-V")))


;; TODO dowcase, uppercase and capitalize, optinal arg
(defun smart-downcase ()
  ""
  (interactive)
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word 1)))


(defun mark-line ()
  "Put mark at end of this line, point at beginning."
  (interactive)
  (back-to-indentation)
  (push-mark (point) nil t)
  (end-of-line))


(defun ido-push-current-directory ()
  "Put the current directory in the ido working directory list"
  (interactive)
  (push default-directory ido-work-directory-list))


(defun create-dir-local-file (DIRECTORY)
  "Create the `dir-locals-file in the DIRECTORY if it does not exist yet."
  (let ((full-dir-locals-file (concat DIRECTORY dir-locals-file)))
    (when (not (file-exists-p full-dir-locals-file))
      ;;(with-temp-file full-dir-locals-file)))
      (write-region "" nil full-dir-locals-file))))


(defun kill-ring-insert ()
  "TODO"
  (interactive)
  (let ((to_insert (completing-read "Yank: "
                                    (delete-duplicates kill-ring :test #'equal))))
    (when (and to_insert (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert to_insert)))


(defun directory-parent (DIR &optional NUMBER)
  "Return the parent directory of `DIR'.
With `NUMBER', return the `NUMBER' parent directory of `DIR'."
  (when DIR
    (if (or (null NUMBER) (= NUMBER 1) (= NUMBER 0))
        (file-name-directory (directory-file-name DIR))
      (directory-parent (file-name-directory (directory-file-name DIR)) (1- NUMBER)))))

(defun copy-directory-if-newer (directory1 directory2)
  "Copy files from `DIRECTORY1' to `DIRECTORY2', but only if the
file already exists and it is older in the latter directory than
in the former."
  (interactive
   (let ((dir (read-directory-name
               "Copy directory: " default-directory default-directory t)))
     (list dir
           (read-directory-name
            (format "Copy directory %s to: " dir)
            default-directory default-directory t))))
  (when (file-directory-p directory2)
    (let ((files-copied 0))
      (dolist (file (directory-files directory1 t))
        (when (file-newer-than-file-p
               file
               (concat (file-name-as-directory directory2) (file-name-nondirectory file)))
          (message "Copying %s to %s" file directory2)
          (copy-file file directory2 t)
          (setq files-copied (1+ files-copied))))
      (message "%d files copied from %s to %s." files-copied directory1 directory2))))
