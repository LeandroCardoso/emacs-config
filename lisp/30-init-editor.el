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


(defun kill-line-and-join (&optional arg)
  "Like `kill-line', but when called from eol, delete all
whitespace except by one."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn
        (kill-line arg)
        (just-one-space))
    (kill-line arg)))


;; Adapted from: https://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "[^[:blank:]]+" "\\&" beg end))


;; settings
(setq-default abbrev-mode t) ; enable abbrev-mode by default
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq kill-whole-line t)
(setq undo-limit (* 1 1024 1024))
(setq undo-strong-limit (truncate (* undo-limit 1.5)))

;; indent.el
(setq tab-always-indent 'complete)

;; simple.el
(setq copy-region-blink-delay 0.25)
(setq delete-pair-blink-delay 0.25)
(setq kill-do-not-save-duplicates t)
(setq normal-erase-is-backspace nil)


;; hooks
(add-hook 'text-mode-hook 'infer-indentation-style)
(add-hook 'prog-mode-hook 'infer-indentation-style)
(add-hook 'conf-mode-hook 'infer-indentation-style)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;; key bindings
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-#") 'base64-encode)
(global-set-key (kbd "C-x C-o") 'delete-all-blank-lines) ; original is delete-blank-lines
(global-set-key (kbd "C-M-|") 'delete-indentation)
(global-set-key (kbd "C-c D") 'delete-pair)
(global-set-key (kbd "C-c <tab>") 'indent-tabs-mode)
(global-set-key [remap kill-line] 'kill-line-and-join)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-h") 'mark-line)                  ; original is help prefix, but we have f1 for it
(global-set-key (kbd "M-<return>") 'newline-no-break)

;; capitalize keys
(global-set-key (kbd "M-c") 'capitalize-dwim) ; original is capitalize-word
(global-set-key (kbd "M-l") 'downcase-dwim)   ; original is downcase-word
(global-set-key (kbd "M-u") 'upcase-dwim)     ; original is upcase-word

;; zap - misc.el
(autoload 'zap-up-to-char "misc")
(global-set-key (kbd "M-z") 'zap-up-to-char) ; original is zap-to-char
