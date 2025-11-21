;;; my-reformatter.el --- my emacs-reformatter definitions -*- lexical-binding: t -*-

;;; Commentary:

;; https://github.com/purcell/emacs-reformatter

;;; Code:

(require 'reformatter)

(defgroup my-reformatter nil
  "My reformatter definitions."
  :prefix "my-reformatter-")

(defun my-reformatter-format ()
  "Format buffer or region.

`major-mode' determines formatter,
see `my-reformatter-formatter-major-mode2lang'.
Formatter functions are: my-reformatter-LANG-(region|buffer)."
  (interactive)
  (call-interactively
   (my-reformatter-get-formatter-func major-mode (use-region-p))))

(defun my-reformatter-get-formatter-func (major-mode-sym for-region)
  "Get interactive function to format region or buffer."
  (let* ((lang (cadr (assoc major-mode-sym my-reformatter-formatter-major-mode2lang)))
         (name (cadr (assoc lang my-reformatter-formatter-lang2names)))
         (fname (format "my-reformatter-%s-format-%s" name (if for-region "region" "buffer")))
         (fsym (intern fname)))
    (if (fboundp fsym) fsym
      (error "Function %s is not defined" fname))))

(defconst my-reformatter-formatter-major-mode2lang
  '((php-mode "php")
    (typescript-mode "typescript")
    (typescript-tsx-mode "typescript")
    (tsx-ts-mode "tsx")
    (js2-mode "javascript")
    (js-jsx-mode "jsx")
    (markdown-mode "markdown")
    (gfm-mode "markdown")
    (sh-mode "shell")
    (ruby-mode "ruby")
    (python-mode "python")
    (lua-mode "lua")
    (cc-mode "clang")
    (c++-mode "clang")
    (c-mode "clang")
    (rust-mode "rust")
    (java-mode "java")
    (go-mode "go")
    (sql-mode "sql")
    (yaml-mode "yaml")
    (json-mode "json")
    (conf-toml-mode "toml")
    (terraform-mode "terraform"))
  "Major mode to language name.")

(defconst my-reformatter-formatter-lang2names
  '(("php" "php")
    ("typescript" "ts")
    ("javascript" "js")
    ("tsx" "ts")
    ("jsx" "js")
    ("markdown" "md")
    ("shell" "sh")
    ("awk" "awk")
    ("ruby" "ruby")
    ("python" "python")
    ("lua" "lua")
    ("clang" "clang")
    ("rust" "rust")
    ("java" "java")
    ("go" "go")
    ("sql" "sql")
    ("yaml" "yaml")
    ("json" "json")
    ("toml" "toml")
    ("terraform" "terraform"))
  "Language name to part of formatter name.")

;; reformatter definitions sholuld have a name of the form
;;   my-reformatter-NAME-format

;; php
;; https://github.com/PHP-CS-Fixer/PHP-CS-Fixer
(reformatter-define my-reformatter-php-format
  :program "php-cs-fixer"
  :args `("fix" "--quiet" ,input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file)
  :lighter " RFphp")

;; typescript
;; https://github.com/denoland/deno
(reformatter-define my-reformatter-ts-format
  :program "deno"
  :args `("fmt" "--ext" "tsx" "--quiet" ,input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file)
  :lighter " RFts")

;; js
;; https://github.com/denoland/deno
(reformatter-define my-reformatter-js-format
  :program "deno"
  :args `("fmt" "--ext" "jsx" "--quiet" ,input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file)
  :lighter " RFjs")

;; markdown
;; https://github.com/denoland/deno
(reformatter-define my-reformatter-md-format
  :program "deno"
  :args `("fmt" "--ext" "md" "--quiet" ,input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file)
  :lighter " RFmd")

;; sh
;; https://github.com/mvdan/sh/tree/master
(reformatter-define my-reformetter-sh-format
  :program "shfmt"
  :args `("--simplify" "--indent" "2")
  :lighter " RFsh")

;; awk
(reformatter-define my-reformatter-awk-format
  :program "awkfmt"
  :lighter " RFawk")

;; terraform
;; https://github.com/hashicorp/terraform
(reformatter-define my-reformatter-terraform-format
  :program "terraform"
  :args `("fmt" "-no-color" "-"))

;; ruby
;; https://github.com/ruby-formatter/rufo
(reformatter-define my-reformatter-ruby-format
  :program "rufo"
  :args `("--simple-exit" "--loglevel=silent")
  :lighter " RFrb")

;; python
;; https://github.com/astral-sh/ruff
(reformatter-define my-reformatter-python-format
  :program "ruff"
  :args `("format" "--silent" ,input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file)
  :lighter " RFpy")

;; lua
;; https://github.com/JohnnyMorganz/StyLua
(reformatter-define my-reformatter-lua-format
  :program "stylua"
  :args `(,input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file)
  :lighter " RFlua")

;; clang
;; https://clang.llvm.org/docs/ClangFormat.html#git-integration
(reformatter-define my-reformatter-clang-format
  :program "clang-format"
  :args `("--style" "Google")
  :lighter " RFclang")

;; rust
;; https://github.com/rust-lang/rustfmt
(reformatter-define my-reformatter-rust-format
  :program "rustfmt"
  :args `("--color" "never" "--quiet")
  :lighter " RFrs")

;; java
;; https://clang.llvm.org/docs/ClangFormat.html#git-integration
(reformatter-define my-reformatter-java-format
  :program "clang-format"
  :args `("--style" "Google")
  :lighter " RFjava")

;; go
;; https://github.com/golang/tools/tree/master/cmd/goimports
(reformatter-define my-reformtter-go-format
  :program "goimports"
  :lighter " RFgo")

;; sql
;; https://github.com/sqlfluff/sqlfluff
(reformatter-define my-reformatter-sql-format
  :program "sqlfluff"
  :args `("format" "--dialect" "mysql" "-")
  :lighter " RFsql")

;; yaml
;; https://github.com/mikefarah/yq
(reformatter-define my-reformatter-yaml-format
  :program "yq"
  :args `("--prettyPrint" "'sort_keys(..)'")
  :lighter " RFyaml")

;; json
;; https://github.com/jqlang/jq
(reformatter-define my-reformatter-json-format
  :program "jq"
  :args `("--sort-keys")
  :lighter " RFjson")

;; toml
;; https://github.com/tamasfe/taplo
(reformatter-define my-reformatter-toml-format
  :program "taplo"
  :args `("format" "--colors" "never" "-")
  :lighter " RFtoml")

(provide 'my-reformatter)
;;; my-reformatter.el ends here
