(with-eval-after-load "calendar"
  ;; calendar.el
  (calendar-set-date-style 'european)

  ;; holidays.el
  (setq holiday-general-holidays nil)
  (setq holiday-christian-holidays nil)
  (setq holiday-hebrew-holidays nil)
  (setq holiday-islamic-holidays nil)
  (setq holiday-bahai-holidays nil)
  (setq holiday-oriental-holidays nil)

  ;; solar.el
  (setq calendar-latitude -23.5)
  (setq calendar-longitude -46.6)
  (setq calendar-location-name "São Paulo, BR")

  ;; cal-dst.el
  )
