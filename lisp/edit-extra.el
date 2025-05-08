;;; edit-extra.el --- Extra editing commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'replace)
(require 'simple)

(defun base64-decode-dwim ()
  "Decode region or whole buffer as base64.

If region is active, decode the region, otherwise decode the whole
buffer.

Return the length of the decoded data.

See `base64-encode-dwim' for details."
  (interactive "*")
  (base64-encode-dwim t))


(defun base64-encode-dwim (&optional arg)
  "Encode region or whole buffer to base64.

If region is active, encode the region, otherwise encode the whole
buffer.

When ARG is present, decode the region or buffer, instead of encode it.

Return the length of the encoded or decoded data.

See `base64-decode-dwim'."
  (interactive "*P")
  (let ((min (if (use-region-p) (region-beginning) (point-min)))
        (max (if (use-region-p) (region-end) (point-max))))
    (if arg
        (base64-decode-region min max)
      (base64-encode-region min max t))))


(defun delete-all-blank-lines ()
  "Delete all blank lines in the buffer.

When region is active delete all blank lines in the region."
  (interactive "*")
  (save-excursion
    (let ((min (if (use-region-p) (region-beginning) (point-min)))
          (max (if (use-region-p) (region-end) (point-max))))
      (flush-lines "^\\s-*$" min max))))


(defun indent-buffer (&optional column)
  "Indent the currently visited buffer.

Indent each nonblank line in the buffer.
A numeric prefix argument specifies a column: indent each line to that column.

With no prefix argument, the command chooses one of these methods and
indents all the lines with it:

  1) If `fill-prefix' is non-nil, insert `fill-prefix' at the
     beginning of each line in the region that does not already begin
     with it.
  2) If `indent-region-function' is non-nil, call that function
     to indent the region.
  3) Indent each line via `indent-according-to-mode'.

Called from a program, START and END specify the region to indent.  If
the third argument COLUMN is an integer, it specifies the column to
indent to; if it is nil, use one of the three methods above.

See `indent-region'."
  (interactive "*P")
  (indent-region (point-min) (point-max) column))


(defcustom infer-indentation-style-region-max 100000
  "The maximum region size for examining indentation style information with
`infer-indentation-style'.  nil means no limit."
  :type 'integer
  :group 'indent)

;; Adapted from: https://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  "Guess the indentation style and install it.

If our source file uses tabs, we use tabs, if spaces spaces, and if
neither, we use the current `indent-tabs-mode'.

Examines in the region up to `infer-indentation-style-region-max'.

This function can be used in hooks to automatically setup:

  (add-hook \='prog-mode-hook \='infer-indentation-style)"
  (interactive)
  (let* ((point-max (min (or infer-indentation-style-region-max (point-max)) (point-max)))
         (space-count (how-many "^  " (point-min) point-max))
         (tab-count (how-many "^\t" (point-min) point-max)))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(defun mark-line (&optional n)
  "Put point at beginning of the current line and mark at end.

With argument N, marks N lines.

Interactively, if this command is repeated or (in Transient Mark mode)
if the mark is active, it marks the next N lines after the ones already
marked."
  (interactive "p")
  (if (and (or (eq last-command this-command) (mark t))
           (and transient-mark-mode mark-active))
      (end-of-line (1+ (or n 1)))
    (back-to-indentation)
    (push-mark (point) nil t)
    (end-of-line n)))


(defun newline-no-break (&optional arg)
  "Insert a new line without breaking.

Insert a newline using the `newline' command after the current line
without breaking.

With ARG, insert that many newlines."
  (interactive "*P")
  (move-end-of-line nil)
  (newline arg t))


(defun kill-line-and-join (&optional arg)
  "Kill the rest of the current line.

If at end of line, kill thru newline and keep just one space.

With prefix argument ARG, kill that many lines from point.  Negative
arguments kill lines backward.  With zero argument, kills the text
before point on the current line.

When calling from a program, nil means \"no arg\", a number counts as a
prefix arg."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn
        (kill-line arg)
        (just-one-space))
    (kill-line arg)))


(defun set-auto-mode+ ()
  "Select major mode appropriate for current buffer.

Differently from `set-auto-mode' this function works on non file
visiting buffers too.

This function may be used as the default value for `major-mode' to
select the major mode appropriate for new buffers not visiting a file:

  (setq-default major-mode \='set-auto-mode-for-major-mode)

See `set-auto-mode'."
  (interactive)
  (if buffer-file-name
      (fundamental-mode)
    (let ((buffer-file-name (buffer-name)))
      (set-auto-mode))))


;; Adapted from: https://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region between BEG and END alphabetically.

With a negative prefix arg, or argument REVERSE is t, sort in reverse
order.

The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (require 'sort)
  (sort-regexp-fields reverse "[^[:blank:]]+" "\\&" beg end))


(provide 'edit-extra)

;;; edit-extra.el ends here
