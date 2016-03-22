(define-generic-mode
    'xfs-form-mode ;; name
  '("//")          ;; comments list
  '(               ;; keywords list
    "ACCESS"
    "ALIGNMENT"
    "BARCODE"
    "CASE"
    "CLASS"
    "COERCIVITY"
    "COLOR"
    "COMMENT"
    "COPYRIGHT"
    "CPI"
    "FOLLOWS"
    "FONT"
    "FOOTER"
    "FORMAT"
    "HEADER"
    "HORIZONTAL"
    "INDEX"
    "INITIALVALUE"
    "KEYS"
    "LANGUAGE"
    "LPI"
    "ORIENTATION"
    "OVERFLOW"
    "POINTSIZE"
    "POSITION"
    "POSITIONONX"
    "POSITIONONY"
    "PRINTAREA"
    "RESTRICTED"
    "RGBCOLOR"
    "SCALING"
    "SIDE"
    "SIZE"
    "SKEW"
    "STYLE"
    "TITLE"
    "TYPE"
    "UNIT"
    "USERPROMPT"
    "VERSION"
    "VERTICAL"
    )
  '(("BEGIN\\|END" . 'font-lock-type-face)
    ("XFSFORM\\|XFSSUBFORM\\|XFSFIELD" . 'font-lock-function-name-face))
  '("\\Form.*\.txt$") ;; auto mode list
  nil                 ;; function list
  )
