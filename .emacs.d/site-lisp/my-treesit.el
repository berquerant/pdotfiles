;;; my-treesit.el --- my tree-sitter settings-*- lexical-binding: t -*-

;;; Code:

(use-package treesit
  :straight (treesit :type built-in)
  :config
  (defun my-treesit-reload-langs ()
    (interactive)
      ;; https://github.com/emacs-tree-sitter/tree-sitter-langs/tree/master/repos
    (setq treesit-language-source-alist
          (with-temp-buffer
            (insert-file-contents (my-getenv-join "DOTFILES_ROOT" ".emacs.d" "treesit-language-source-alist.el"))
            (goto-char (point-min))
            (read (current-buffer))))
    ;; https://zenn.dev/glassonion1/articles/20752bb8d2cf98
    (dolist (element treesit-language-source-alist)
      (let ((lang (car element)))
        (if (treesit-language-available-p lang)
            (message "treesit: %s is already installed" lang)
          (warn "treesit: %s is not installed" lang)
          (treesit-install-language-grammar lang)))))
  (my-treesit-reload-langs)
  :custom
  (treesit-font-lock-level 4))

(provide 'my-treesit)
;;; my-treesit.el ends here
