;;; my-time.el --- my time utilities -*- lexical-binding: t -*-

;;; Commentary:

;; misc utilities about time

;;; Code:

(defgroup my-time nil
  "My misc utilities about time."
  :prefix "my-time-")

(defun my-time-datetime (&optional time zone)
  "Convert TIME (timestamp) in ZONE (timezone) into string.
default TIME is now, ZONE is here."
  (format-time-string "%F %T"
                      (or time (current-time))
                      (or zone (current-time-zone))))

;;;###autoload
(defun my-time-display-datetime (&optional time)
  "Display TIME (timestamp) as string.
default time is now.
time zone is here."
  (interactive (list (read-number "Timestamp: " (my-time-timestamp))))
  (message (my-time-datetime time)))

(defun my-time-timestamp (&optional time)
  "Convert TIME (time-string) into timestamp.
default TIME is now.
time zone is here."
  (let ((ts (or (and time (apply #'encode-time (parse-time-string time)))
                (current-time))))
    (+ (* (car ts) (expt 2 16)) (cadr ts))))

;;;###autoload
(defun my-time-display-timestamp (&optional time)
  "Display TIME (time-string) as timestamp.
default time is now.
time zone is here."
  (interactive (list (read-string "Time: " (my-time-datetime))))
  (message "%d" (floor (my-time-timestamp time))))

(defun my-time-timer (duration hook)
  "Invoke HOOK after DURATION seconds.
DURATION is not negative, HOOK has no arguments."
  (run-at-time (or (and (>= duration 0) duration) 0)
               nil hook))

(provide 'my-time)
;;; my-time.el ends here
