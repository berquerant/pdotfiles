;;; my-flycheck-golangci-lint.el -- golangci-lint -*- lexical-binding: t -*-

;;; Code:

(require 'flycheck)

(flycheck-define-checker my-golangci-lint
  "golangci-lint."
  :command
  ("golangci-lint"
   "run"
   "--output.checkstyle.path=stdout"
   "--output.text.path=stderr"
   "--path-mode=abs"
   ".")
  :error-parser flycheck-parse-checkstyle
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) line-end)
   (error line-start (file-name) ":" line ":" (message) line-end))
  :modes (go-mode))

;;;###autoload
(defun my-flycheck-golangci-lint-setup ()
  (interactive)
  (add-to-list 'flycheck-checkers 'my-golangci-lint))

(provide 'my-flycheck-golangci-lint)
;;; my-flycheck-golangci-lint.el ends here
