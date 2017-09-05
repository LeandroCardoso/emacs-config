(setq isearch-allow-scroll t)

(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-max-at-a-time nil)

;; http://endlessparentheses.com/better-backspace-during-isearch.html
;; https://gist.github.com/johnmastro/508fb22a2b4e1ce754e0
(defun isearch-delete-something ()
  "Delete non-matching text or the last character."
  ;; Mostly copied from `isearch-del-char' and Drew's answer on the page above
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string (substring isearch-string 0
                                    (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(defun isearch-yank-symbol ()
  "Pull next symbol from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda() (forward-symbol 1) (point))))

(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
;;(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-something) ;; original is isearch-delete-char
;;(define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)            ;; original is isearch-del-char
