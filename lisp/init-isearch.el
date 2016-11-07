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

(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)

;;original is isearch-delete-char
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-something)
