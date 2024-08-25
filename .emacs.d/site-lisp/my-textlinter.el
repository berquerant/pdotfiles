;;; my-textlinter.el --- textlint -*- lexical-binding: t -*-

;;; Code:

(require 'little-async)

(defgroup my-textlinter nil
  "My textlint,"
  :prefix "my-textlinter-")

(defcustom my-textlinter-command "textlinter"
  "textlinter command."
  :type 'string)

(defcustom my-textlinter-process-name "my-textlinter"
  "textlinter process name."
  :type 'string)

(defcustom my-textlinter-output-buffer-name "*my-textlinter-output*"
  "textlinter output buffer name."
  :type 'string)

(defun my-textlinter--insert-output-buffer (input)
  "Insert INPUT into buffer `my-textlinter-output-buffer-name'."
  (with-current-buffer (get-buffer-create my-textlinter-output-buffer-name)
    (goto-char (point-max))
    (insert input))
  (display-buffer my-textlinter-output-buffer-name))

(defun my-textlinter--output-filter (p output)
  "textlinter output process filter."
  (my-textlinter--insert-output-buffer output))

;;;###autoload
(defun my-textlinter-do (txt)
  "Textlint TXT."
  (little-async-start-process '("textlinter")
                              :input txt
                              :process-name my-textlinter-process-name
                              :buffer-name my-textlinter-output-buffer-name
                              :filter 'my-textlinter--output-filter))

(provide 'my-textlinter)
;;; my-textlinter.el ends here
