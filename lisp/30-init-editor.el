;; functions

(defun indent-buffer (&optional column)
  "Indent the currently visited buffer.
A numeric prefix argument specifies a column: indent each line to that column.

See `indent-region'"
  (interactive "*P")
  (indent-region (point-min) (point-max) column))


(defun indent-defun ()
  "Indent the current function.
See `indent-region'"
  (interactive "*")
  (save-excursion
    (let ((begin nil)
          (end nil))
      (beginning-of-defun)
      (setq begin (point))
      (end-of-defun)
      (setq end (point))
      (indent-region begin end))))

(defvar infer-indentation-style-region-max 100000
  "The maximum region size for examining indentation style with
`infer-indentation-style'. nil means no limit.")

;; Adapted from: https://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  "If our source file uses tabs, we use tabs, if spaces spaces,
and if neither, we use the current `indent-tabs-mode'"
  (interactive)
  (let* ((point-max (or infer-indentation-style-region-max (point-max)))
         (space-count (how-many "^  " (point-min) point-max))
         (tab-count (how-many "^\t" (point-min) point-max)))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(defun mark-line (&optional N)
  "Put mark at end of this line, point at beginning.
With argument N not nil or 1, move forward N - 1 lines first."
  (interactive "P")
  (back-to-indentation)
  (push-mark (point) nil t)
  (end-of-line N))


(defun newline-no-break (&optional arg)
  "Insert a newline using the `newline' command after the current line without breaking.
With ARG, insert that many newlines."
  (interactive "*P")
  (move-end-of-line nil)
  (newline arg))


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

(add-to-list 'text-mode-hook 'infer-indentation-style)
(add-to-list 'prog-mode-hook 'infer-indentation-style)

;; key bindings
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "M-RET") 'newline-no-break)
(global-set-key (kbd "C-h") 'mark-line) ;; default is help prefix, but we have f1 for it
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-M-|") 'delete-indentation)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-u") 'upcase-dwim) ;; default is upcase-word
(global-set-key (kbd "M-l") 'downcase-dwim) ;; default is downcase-word
(global-set-key (kbd "M-c") 'capitalize-dwim) ;; default is capitalize-word
(global-set-key (kbd "C-c D") 'delete-pair)

;; zap - misc.el
(autoload 'zap-up-to-char "misc")
(global-set-key (kbd "M-z") 'zap-up-to-char) ;; default is zap-to-char
