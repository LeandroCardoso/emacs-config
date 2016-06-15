;; ispell
(setq ispell-dictionary "en_US")
(setq ispell-help-in-bufferp 'electric)
(setq ispell-program-name "hunspell")
(setq ispell-query-replace-choices t)
(setq ispell-silently-savep t)

(when (eq system-type 'windows-nt)
  ;; 'look' is not automatically used because we have a custom unix path in windows.
  (setq ispell-look-p t)
  ;; use custom user dictionary in windows
  (setq ispell-complete-word-dict (expand-file-name "~/dict/en_US.txt"))
  (setenv "DICPATH" (expand-file-name "~/dict"))
  (setenv "DICTIONARY" "en_US"))


;; flyspell
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-persistent-highlight nil) ;; make flyspell less annoying

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


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
