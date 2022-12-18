;;; my-sticky-buffer-mode.el --- make sticky buffers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

;;;###autoload
(define-minor-mode my-sticky-buffer-mode
  "Make the current window always display this buffer."
  :init-value nil
  :lighter " sticky"
  :key-map nil
  (set-window-dedicated-p (selected-window) my-sticky-buffer-mode))

(provide 'my-sticky-buffer-mode)
;;; my-sticky-buffer-mode.el ends here
