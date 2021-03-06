;; functions

(defun base64-decode ()
  "If region is active, base64-decode the region, otherwise
base64-decode the whole buffer."
  (interactive)
  (base64-encode t))


(defun base64-encode (&optional arg)
  "When ARG is omitted and if region is active, base64-encode the
region, otherwise base64-encode the whole buffer.

When ARG is present and if region is active, base64-decode the
region, otherwise base64-decode the whole buffer."
  (interactive "*P")
  (let ((min (if (use-region-p) (region-beginning) (point-min)))
        (max (if (use-region-p) (region-end) (point-max))))
    (if arg
        (base64-decode-region min max)
      (base64-encode-region min max t))))


(defun indent-buffer (&optional column)
  "Indent the currently visited buffer.
A numeric prefix argument specifies a column: indent each line to that column.

See `indent-region'"
  (interactive "*P")
  (indent-region (point-min) (point-max) column))


(defvar infer-indentation-style-region-max 100000
  "The maximum region size for examining indentation style with
`infer-indentation-style'. nil means no limit.")

;; Adapted from: https://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  "If our source file uses tabs, we use tabs, if spaces spaces,
and if neither, we use the current `indent-tabs-mode'"
  (interactive)
  (let* ((point-max (min (or infer-indentation-style-region-max (point-max)) (point-max)))
         (space-count (how-many "^  " (point-min) point-max))
         (tab-count (how-many "^\t" (point-min) point-max)))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(defun insert-from-kill-ring (&optional arg)
  "Search a past killed text from the `kill-ring' and insert it.
Put point at the end, and set mark at the beginning without
activating it. When ARG is non nil, put point at beginning, and
mark at end.

This command honors the `yank-handled-properties' and
`yank-excluded-properties' variables, and the `yank-handler' text
property, as described by the command `yank' (\\[yank])."
  (interactive "P*")
  (let ((text (completing-read "Insert: " (seq-uniq kill-ring))))
    (push-mark)
    (insert-for-yank text)
    (if (consp arg)
        ;; This is like exchange-point-and-mark, but doesn't activate the mark.
        ;; It is cleaner to avoid activation, even though the command
        ;; loop would deactivate the mark because we inserted text.
        (goto-char (prog1 (mark t)
                     (set-marker (mark-marker) (point) (current-buffer))))))
  nil)


(defun mark-line (&optional n)
  "Put point at beginning of the current line and mark at end.

With argument N, marks N lines.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next N lines after the
ones already marked."
  (interactive "p")
  (if (and (or (eq last-command this-command) (mark t))
           (and transient-mark-mode mark-active))
      (end-of-line (1+ (or n 1)))
    (back-to-indentation)
    (push-mark (point) nil t)
    (end-of-line n)))


(defun newline-no-break (&optional arg)
  "Insert a newline using the `newline' command after the current line without breaking.
With ARG, insert that many newlines."
  (interactive "*P")
  (move-end-of-line nil)
  (newline arg t))


;; Adapted from: https://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "[^[:blank:]]+" "\\&" beg end))


(defun toggle-indent-tabs-mode ()
  "Toggle the value of `indent-tabs-mode'."
  (interactive)
  (message "indent-tabs-mode: %S" (setq indent-tabs-mode (not indent-tabs-mode))))


;; settings
(setq-default abbrev-mode t) ;; enable abbrev-mode by default
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq kill-whole-line t)

(add-hook 'text-mode-hook 'infer-indentation-style)
(add-hook 'prog-mode-hook 'infer-indentation-style)
(add-hook 'conf-mode-hook 'infer-indentation-style)

;; key bindings
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "<C-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-#") 'base64-encode)
(global-set-key (kbd "C-M-|") 'delete-indentation)
(global-set-key (kbd "C-c D") 'delete-pair)
(global-set-key (kbd "C-M-y") 'insert-from-kill-ring)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-h") 'mark-line) ;; default is help prefix, but we have f1 for it
(global-set-key (kbd "M-RET") 'newline-no-break)

(global-set-key (kbd "M-c") 'capitalize-dwim) ;; default is capitalize-word
(global-set-key (kbd "M-l") 'downcase-dwim) ;; default is downcase-word
(global-set-key (kbd "M-u") 'upcase-dwim) ;; default is upcase-word


;; zap - misc.el
(autoload 'zap-up-to-char "misc")
(global-set-key (kbd "M-z") 'zap-up-to-char) ;; default is zap-to-char
