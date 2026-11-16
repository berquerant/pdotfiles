;;; my-reformatter.el --- my emacs-reformatter definitions -*- lexical-binding: t -*-

;;; Commentary:

;; https://github.com/purcell/emacs-reformatter

;;; Code:

(require 'reformatter)

(defgroup my-reformatter nil
  "My reformatter definitions."
  :prefix "my-reformatter-")

(defconst my-reformatter-mode-alist
  '((php-mode . php)
    (hcl-mode . hcl)
    (typescript-mode . ts)
    (typescript-tsx-mode . ts)
    (typescript-ts-mode . ts)
    (tsx-ts-mode . ts)
    (js2-mode . js)
    (js-jsx-mode . js)
    (markdown-mode . md)
    (gfm-mode . md)
    (sh-mode . sh)
    (bash-ts-mode . sh)
    (awk-mode . awk)
    (ruby-mode . ruby)
    (ruby-ts-mode . ruby)
    (python-mode . python)
    (python-ts-mode . python)
    (lua-mode . lua)
    (lua-ts-mode . lua)
    (cc-mode . clang)
    (c++-mode . clang)
    (c-mode . clang)
    (c-ts-mode . clang)
    (c++-ts-mode . clang)
    (rust-mode . rust)
    (rust-ts-mode . rust)
    (java-mode . java)
    (java-ts-mode . java)
    (go-mode . go)
    (go-ts-mode . go)
    (sql-mode . sql)
    (yaml-mode . yaml)
    (yaml-ts-mode . yaml)
    (json-mode . json)
    (json-ts-mode . json)
    (conf-toml-mode . toml)
    (toml-ts-mode . toml)
    (terraform-mode . terraform))
  "Alist mapping major mode to reformatter name.")

(defun my-reformatter-get-formatter-func (mode for-region)
  "Get interactive function to format region or buffer for MODE."
  (if-let* ((name (alist-get mode my-reformatter-mode-alist))
            (fname (format "my-reformatter-%s-format-%s" name (if for-region "region" "buffer")))
            (fsym (intern fname)))
      (if (fboundp fsym) fsym
        (error "Function %s is not defined" fname))
    (error "No reformatter configured for %s" mode)))

(defun my-reformatter-format ()
  "Format buffer or region based on `major-mode'."
  (interactive)
  (call-interactively
   (my-reformatter-get-formatter-func major-mode (use-region-p))))

;; Reformatter definitions (my-reformatter-NAME-format)
(reformatter-define my-reformatter-php-format
  :program "php-cs-fixer"
  :args `("fix" "--quiet" ,input-file)
  :stdin nil :stdout nil :input-file (reformatter-temp-file))

(reformatter-define my-reformatter-ts-format
  :program "prettier"
  :args `("--stdin-filepath" ,(or buffer-file-name "file.tsx")))

(reformatter-define my-reformatter-js-format
  :program "prettier"
  :args `("--stdin-filepath" ,(or buffer-file-name "file.jsx")))

(reformatter-define my-reformatter-md-format
  :program "prettier"
  :args `("--stdin-filepath" ,(or buffer-file-name "file.md")))

(reformatter-define my-reformatter-sh-format
  :program "shfmt"
  :args '("--simplify" "--indent" "2"))

(reformatter-define my-reformatter-awk-format
  :program "awkfmt")

(reformatter-define my-reformatter-terraform-format
  :program "terraform"
  :args '("fmt" "-no-color" "-"))

(reformatter-define my-reformatter-ruby-format
  :program "rufo"
  :args '("--simple-exit" "--loglevel=silent"))

(reformatter-define my-reformatter-python-format
  :program "ruff"
  :args `("format" "--silent" ,input-file)
  :stdin nil :stdout nil :input-file (reformatter-temp-file))

(reformatter-define my-reformatter-lua-format
  :program "stylua"
  :args `(,input-file)
  :stdin nil :stdout nil :input-file (reformatter-temp-file))

(reformatter-define my-reformatter-clang-format
  :program "clang-format"
  :args '("--style" "Google"))

(reformatter-define my-reformatter-rust-format
  :program "rustfmt"
  :args '("--color" "never" "--quiet"))

(reformatter-define my-reformatter-java-format
  :program "clang-format"
  :args '("--style" "Google"))

(reformatter-define my-reformatter-go-format
  :program "goimports")

(reformatter-define my-reformatter-sql-format
  :program "sqlfluff"
  :args '("format" "--dialect" "mysql" "-"))

(reformatter-define my-reformatter-yaml-format
  :program "yq"
  :args '("--prettyPrint" "'sort_keys(..)'"))

(reformatter-define my-reformatter-json-format
  :program "jq"
  :args '("--sort-keys"))

(reformatter-define my-reformatter-toml-format
  :program "taplo"
  :args '("format" "--colors" "never" "-"))

(reformatter-define my-reformatter-hcl-format
  :program "hclfmt"
  :args `(,input-file))

(provide 'my-reformatter)
;;; my-reformatter.el ends here
