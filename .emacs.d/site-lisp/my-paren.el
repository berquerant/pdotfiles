;;; my-paren.el --- my paren utilities -*- lexical-binding: t -*-

;;; Code:

(provide 'my-paren)

(defun my-paren-surround-region (start end left right)
  "Surround region with specified strings.
Insert LEFT to the beginning of the region, RIGHT to the end of the region."
  (interactive "r\nsLeft: \nsRight: ")
  (let ((region-text (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert (concat left
                    region-text
                    right))))

(defun my-paren--search-parens (left right)
  "Search for LEFT backwards and RIGHT forwards.
Return the starting position of each string if found.
Result is (LEFT_START_POS . RIGHT_START_POS)."
  (when (or (string-empty-p left) (string-empty-p right))
    (error "Both left and right must be non-empty strings."))
  (let ((start-pos (point))
        (end-pos (point)))

    (save-excursion
      (setq start-pos (search-backward left nil t))
      (when (null start-pos)
        (error "Left %s not found." left)))

    (save-excursion
      (setq end-pos (search-forward right nil t))
      (when (null end-pos)
        (error "Right %s not found." right)))

    (cons start-pos (- end-pos (length right)))))

(defun my-paren-remove-parens (left right)
  "If search for LEFT backwards and RIGHT forwards and found, delete them."
  (interactive "sLeft: \nsRight: ")
  (let* ((p (my-paren--search-parens left right))
         (start-pos (car p))
         (end-pos (cdr p)))
    (delete-region end-pos (+ end-pos (length right)))
    (delete-region start-pos (+ start-pos (length left)))))

(defun my-paren-kill-sexp (left right)
  "If search for LEFT backwards and RIGHT forwards and found, kill them and string surrounded by them."
  (interactive "sLeft: \nsRight: ")
  (let* ((p (my-paren--search-parens left right))
         (start-pos (car p))
         (end-pos (cdr p)))
    (kill-region start-pos (+ end-pos (length right)))))

(provide 'my-paren)
;;; my-paren.el ends here
