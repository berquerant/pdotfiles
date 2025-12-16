;;; my-flycheck-golangci-lint.el -- golangci-lint -*- lexical-binding: t -*-

;;; Code:

(require 'flycheck)

(flycheck-define-checker my-golangci-lint
  "golangci-lint."
  :command
  ("flycheck-golangci-lint.sh")
  :error-parser flycheck-parse-checkstyle
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) line-end)
   (error line-start (file-name) ":" line ":" (message) line-end))
  :modes (go-mode))

;;;###autoload
(defun my-flycheck-golangci-lint-setup ()
  (add-to-list 'flycheck-checkers 'my-golangci-lint))

(provide 'my-flycheck-golangci-lint)
;;; my-flycheck-golangci-lint.el ends here
