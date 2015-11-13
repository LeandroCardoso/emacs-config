(defun w32-service-running (SERVICE)
  "Query if a Windows SERVICE is running.
Return non-nil if SERVICE is running, nil otherwise"
  (interactive "sService: ")
  (when SERVICE
    (with-temp-buffer
      (call-process "net" nil t nil "start")
      (goto-char (point-min))
      (setq case-fold-search t)
      (re-search-forward (concat "^ *" (regexp-quote SERVICE) "$") nil t))))

(defun w32-service-start (SERVICE &optional ARG)
  "Start, stop or restart a Windows SERVICE
The ARG parameter controls the behaviour
  nil or 0 : start
  1 : stop
  2 : restart"
  (interactive "sService: P")
  (when SERVICE
    (call-process "net" nil "*Services*" nil
                  (cond
                   ((or (null ARG) (= ARG 0)) "start")
                   ((or (= ARG 1) (= ARG 2)) "stop"))
                  SERVICE)
    (when (equal ARG 2)
      (w32-service-start SERVICE))))

(defun w32-service-stop (SERVICE)
  "Stop a Windows SERVICE."
  (interactive "sService: ")
  (w32-service-start SERVICE 1))

(defun w32-service-restart (SERVICE)
  "Restart a Windows SERVICE."
  (interactive "sService: ")
  (w32-service-start SERVICE 2))
