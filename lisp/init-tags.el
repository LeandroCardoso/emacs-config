(defadvice find-tag (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window))

(defadvice find-tag-other-window (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window))

(defadvice find-tag-other-frame (after find-tag-and-reposition-window)
  "Reposition window after find a tag"
  (reposition-window))

(ad-activate 'find-tag)
(ad-activate 'find-tag-other-window)
(ad-activate 'find-tag-other-frame)
