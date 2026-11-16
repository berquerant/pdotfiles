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

(defun my-time-timestamp (&optional time)
  "Convert TIME (time-string) into timestamp.
default TIME is now.
time zone is here."
  (let ((ts (or (and time (apply #'encode-time (parse-time-string time)))
                (current-time))))
    (+ (* (car ts) (expt 2 16)) (cadr ts))))

(provide 'my-time)
;;; my-time.el ends here
