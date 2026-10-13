#!/bin/bash
set -euo pipefail

# emacs-keyfreq-json.sh:
# Convert Emacs .keyfreq S-expression data into JSON format.

readonly freq="${1:-${EMACSD:-${HOME}/.emacs.d}/.keyfreq}"

if [[ ! -f "${freq}" ]]; then
  echo "Error: Keyfreq file not found: ${freq}" >&2
  exit 1
fi

KEYFREQ_TARGET="${freq}" emacs --batch --quick --eval '
(progn
  (require '\''json)
  (let ((path (getenv "KEYFREQ_TARGET")))
    (with-temp-buffer
      (insert-file-contents path)
      (let* ((data (read (current-buffer)))
             (normalized
              (mapcar
               (lambda (item)
                 (let* ((key (car item))
                        (mode (car key))
                        (cmd (cdr key))
                        (count (cdr item)))
                   `((mode . ,(format "%s" mode))
                     (command . ,(format "%s" cmd))
                     (count . ,count))))
               data)))
        (princ (json-encode normalized))))))
'
