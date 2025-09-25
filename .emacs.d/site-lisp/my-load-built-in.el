;;; my-load-built-in.el --- Load some built-in packages -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package browse-url
  :straight (browse-url :type built-in)
  :bind
  ("M-#" . browse-url-at-point))

(use-package goto-addr
  :demand t
  :straight (goto-addr :type built-in)
  :bind
  (:map goto-address-highlight-keymap
        ("M-#" . goto-address-at-point))
  :config
  (unbind-key "C-c RET" goto-address-highlight-keymap)
  (global-goto-address-mode))

(use-package ediff
  :straight (ediff :type built-in)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package generic-x
  :straight (generic-x :type built-in)
  :commands (generic-mode-hook run-generic-mode-hook)
  :hook
  (change-major-mode . run-generic-mode-hook)
  :config
  (defconst generic-mode-regex ".*-generic-mode")
  (defun generic-mode-p ()
    "Match 'major-mode' with 'generic-mode-regex'."
    (string-match-p generic-mode-regex (symbol-name major-mode)))
  (defvar generic-mode-hook nil
    "Normal hook run when entering some Generic mode.")
  (defun run-generic-mode-hook ()
    "Call hooks."
    (when (generic-mode-p)
      (cl-loop for h in generic-mode-hook
            do (funcall h)))))

(use-package nxml-mode
  :straight (nxml-mode :type built-in)
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.xsl\\'" . nxml-mode)
         ("\\.xhtml\\'" . nxml-mode))
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-attribute-indent 2)
  (nxml-child-indent 2)
  (nxml-char-ref-display-glyph-flag nil)
  :hook
  (nxml-mode . (lambda ()
                 (setq indent-tabs-mode nil)
                 (setq tab-width 2))))

;; https://www.emacswiki.org/emacs/WhiteSpace
(use-package whitespace
  :straight (whitespace :type built-in)
  :diminish ((global-whitespace-mode . "")
             (whitespace-mode . ""))
  :custom-face
  (whitespace-trailing ((t (:background "#232323"))))
  (whitespace-empty ((t (:background "#232323"))))
  (whitespace-tab ((t (:foreground "DimGray" :background "#232323"))))
  (whitespace-space ((t (:foreground "DimGray" :background "#232323"))))
  (whitespace-newline ((t (:foreground "DimGray" :background "#232323"))))
  :custom
  (whitespace-style '(face
                      trailing
                      tabs
                      hspace
                      spaces
                      space-mark
                      tab-mark
                      newline
                      newline-mark
                      empty))
  (whitespace-display-mappings
   '((space-mark ?\u3000 [?\u25a1] [?\u1427])
     (space-mark ?\xA0   [?\u00a4] [?_])
     (newline-mark ?\n   [?\u21b5 ?\n])
     ;; WARNING: the mapping below has a problem.
     ;; When a TAB occupies exactly one column, it will display the
     ;; character ?\xBB at that column followed by a TAB which goes to
     ;; the next TAB column.
     ;; If this is a problem for you, please, comment the line below.
     (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (whitespace-space-regexp "\\(\u3000+\\)") ; full-width-space
  :config
  (global-whitespace-mode t)
  (my-macro-thyristor delete-trailing-whitespace)
  (add-to-list 'before-save-hook 'delete-trailing-whitespace-thyristor)
  (bind-key "M-m M-m M-m" 'delete-trailing-whitespace-thyristor-toggle))

(use-package dired
  :straight (dired :type built-in)
  :config
  (setq insert-directory-program "gls") ; use coreutils ls to use ls --dired
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-isearch-filenames t))

(use-package dired-x
  :after dired
  :straight (dired-x :type built-in)
  :bind
  ("C-x j d" . dired-jump))

(provide 'my-load-built-in)
;;; my-load-built-in.el ends here
