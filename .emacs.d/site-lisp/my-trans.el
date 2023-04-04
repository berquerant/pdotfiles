;;; my-trans.el --- translation -*- lexical-binding: t -*-

;;; Commentary:

;; my emacs integration of https://github.com/soimort/translate-shell

;;; Code:

(defgroup my-trans nil
  "My google translation."
  :prefix "my-trans-")

(defcustom my-trans-command "trans"
  "trans command."
  :type 'string)

(defcustom my-trans-process-name "my-trans"
  "trans process name."
  :type 'string)

(defcustom my-trans-output-buffer-name "*my-trans-output*"
  "translation output buffer name."
  :type 'string)

(defun my-trans--insert-output-buffer (input)
  "Insert INPUT into buffer `my-trans-output-buffer-name'."
  (with-current-buffer (get-buffer-create my-trans-output-buffer-name)
    (goto-char (point-max))
    (insert input)))

(defun my-trans--trans-output-filter (p output)
  "translation output process filter."
  (my-trans--insert-output-buffer output))

(defun my-trans-trans (src dest txt)
  "Translate TXT (as lang SRC) into lang DEST."
  (my-trans--insert-output-buffer (format "[%s]\n%s\n[%s]\n" src txt dest))
  (little-async-start-process (format "trans -b -s %s -t %s" src dest)
                              :input txt
                              :process-name my-trans-process-name
                              :buffer-name my-trans-output-buffer-name
                              :filter 'my-trans--trans-output-filter))

;;;###autoload
(defun my-trans-into-ja (txt)
  "Translate TXT into Japanese."
  (my-trans-trans "auto" "ja" txt))

;;;###autoload
(defun my-trans-into-en (txt)
  "Translate TXT into English."
  (my-trans-trans "auto" "en" txt))

(provide 'my-trans)
;;; my-trans.el ends here
