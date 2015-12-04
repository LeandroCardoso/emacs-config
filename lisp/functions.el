(require 'woman)
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
  (let ((old-def-dir default-directory)
        (cd-absolute DIR)
        (message (concat "Creating TAGS at " DIR))
        (call-process "ctags" nil "*Messages*" nil "-e" "-R" "--extra=+q" "--languages=c++")
        (cd-absolute old-def-dir))))


(defun force-backup-buffer ()
  "Force a backup of the disk file visisted by the current buffer
This is normally done before saving the buffer the first time.

See `backup-buffer'"
  (interactive)
  (setq buffer-backed-up nil))


(defun dired-move-to-filename-i ()
  "Move to the beginning of the filename on the current line.
Return the position of the beginning of the filename, or nil if none found.
This is the interactive version of `dired-move-to-filename'"
  (interactive)
  (dired-move-to-filename))


;; TODO dowcase, uppercase and capitalize, optinal arg
(defun smart-downcase ()
  ""
  (interactive)
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word 1)))

;; from http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(defun ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

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

(defun set-custom-frame-title ()
  (setq frame-title-format
        (list "emacs"
              (when (boundp 'desktop-dirname)
                (list " / " (file-name-nondirectory (directory-file-name desktop-dirname)))))))

(defun create-dir-local-file (DIRECTORY)
  "Create the `dir-locals-file in the DIRECTORY if it does not exist yet."
  (let ((full-dir-locals-file (concat DIRECTORY dir-locals-file)))
    (when (not (file-exists-p full-dir-locals-file))
      ;;(with-temp-file full-dir-locals-file)))
      (write-region "" nil full-dir-locals-file))))
