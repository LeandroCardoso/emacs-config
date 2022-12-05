(require 'pulse)
(require 'nxml-mode)
(require 'xml-format)

(defcustom fragment-search-distance 15000
  "Maximum distance used to search for fragments by `fragment-display-other-window'"
  :type '(choice (integer :tag "Number of characters")
                 (const :tag "Unlimited" nil))
  :group 'fragment)

;; TODO keep source highlight
;; TODO inclusive
;; TODO act on region
(defun fragment-display-other-window (begin-regexp end-regexp &optional exclude post-function)
  "Display a text fragment delimited between BEGIN-REGEXP and
END-REGEXP in a temporary buffer in another window.

The text fragment will be momentary highlighted and it must be at
least partially visible in the current window. The text matched
by BEGIN-REGEXP and END-REGEXP are part of the text fragment,
unless the parameter EXCLUDE is t. The search distance is
customized by the `fragment-search-distance'. After the text
fragment is inserted in the new buffer, the POST-FUNCTION is
evaluated in the temporary buffer.

Returns the text fragment or nil when not found."
  (save-mark-and-excursion
    (let* ((win-beg (save-excursion (move-to-window-line 0) (point)))
           (win-end (save-excursion (move-to-window-line -1) (end-of-line) (point)))
           (match-back-beg (save-excursion
                             (end-of-line)
                             (re-search-backward begin-regexp (- (point) fragment-search-distance) t)))
           (match-back-end (when match-back-beg
                             (save-excursion
                               (goto-char match-back-beg)
                               (re-search-forward end-regexp (+ (point) fragment-search-distance) t))))
           (match-back-found (and match-back-end (> match-back-end win-beg)))
           (match-forw-beg (unless match-back-found
                             (save-excursion
                               (end-of-line)
                               (when (re-search-forward begin-regexp win-end t)
                                 (match-beginning 0)))))
           (match-forw-end (when match-forw-beg
                             (save-excursion
                               (goto-char match-forw-beg)
                               (re-search-forward end-regexp (+ (point) fragment-search-distance) t))))
           (match-forw-found match-forw-end)
           (match (if match-back-found
                       (cons match-back-beg  match-back-end)
                    (if match-forw-found
                        (cons match-forw-beg  match-forw-end)
                      '(nil . nil))))
           (beg (car match))
           (end (cdr match))
           (fragment (when (and beg end) (buffer-substring-no-properties beg end))))
      (when fragment
        (with-current-buffer-window
            (format "*%s/%s*" element (or (uniquify-buffer-base-name) (buffer-name)))
            nil nil
          (insert fragment)
          (funcall post-function))
        (pulse-momentary-highlight-region beg end))
      fragment)))

(defun fragment-xml-display-other-window (element)
  "Search for a visible XML fragment with root ELEMENT in the
current buffer and display it in a temporary buffer in another
window.

See `fragment-display-other-window'"
  (interactive)
  (fragment-display-other-window (format "<%s\\(?: [^>]*>\\|>\\)" element)
                                 (format "</%s>" element)
                                 nil
                                 '(lambda ()
                                    (xml-mode)
                                    (xml-format))))

(provide 'fragment)
