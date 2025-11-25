;;; init.el --- Initialize Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; If EMACS_DEBUG_PROFILER is not empty, enable profiler.
;; If EMACS_DEBUG_TRACE_AFTER_LOAD is not empty, print loaded files.

;; for macOS

;;; Code:

;; profilers init

(defconst my-profiler-enabled
  (and (getenv "EMACS_DEBUG_PROFILER") t))
(when my-profiler-enabled
  (require 'profiler)
  (profiler-start 'cpu+mem)
  (defun my-profiler-report ()
    "Report profiling result when `my-profiler-enabled'."
    (profiler-stop)
    (profiler-report))
  (add-hook 'emacs-startup-hook 'my-profiler-report))

(setq elp-sort-by-function 'elp-sort-by-average-time)

(defun display-emacs-init-time()
  "Display the elapsed time to initialize."
  (message "init time: %s" (emacs-init-time)))
(add-hook 'emacs-startup-hook 'display-emacs-init-time)

(defun trace-after-load-function (loaded-file)
  (message "[trace-after-load-function] %s" loaded-file))
(let ((debug-trace-after-load (or (getenv "EMACS_DEBUG_TRACE_AFTER_LOAD") "")))
  (unless (string-equal debug-trace-after-load "")
    (add-hook 'after-load-functions #'trace-after-load-function)))

;; profilers init ends here

(setq user-emacs-directory (expand-file-name user-emacs-directory)) ; into absolute path
;; install and initialize package manager

(defvar native-comp-deferred-compilation-deny-list nil) ; Workaround: Symbol's value as variable is void: native-comp-deferred-compilation-deny-list
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("stable" . "https://stable.melpa.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities
                        '(("melpa" . 100)
                          ("stable" . 90)
                          ("gnu" . 80)
                          ("gnu-devel" . 70)
                          ("nongnu" . 60)))
(straight-use-package 'use-package) ; use-package integration

(defconst my-straight-profile (concat user-emacs-directory "straight-default.el"))
(defconst my-straight-directory (concat straight-base-dir "straight"))

(setq use-package-verbose t                  ; enable logs in *Message* buffer
      use-package-minimum-reported-time 0.2
      use-package-compute-statistics t       ; see use-package-report
      straight-profiles `((nil . ,my-straight-profile))
      straight-use-package-by-default t      ; use-package integration
      byte-compile-warnings '(cl-functions)  ; ignore package cl is deprecated
      warning-suppress-types '((comp)))      ; do not display comp warnings immediately

(use-package bind-key) ; easy key bindings
(use-package diminish) ; diminished mode-line
(use-package cl-lib)
(use-package s)
(use-package f)
(use-package exec-path-from-shell
  :demand t
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package tramp
  :straight (tramp :type built-in)
  :config
  ;; for devcontainer (golang)
  (add-to-list 'tramp-remote-path "/go/bin")
  (add-to-list 'tramp-remote-path "/usr/local/go/bin")
  ;; for devcontainer (rust)
  (add-to-list 'tramp-remote-path "/usr/local/cargo/bin"))

(defun my-getenv (&optional arg)
  "Get environment variable ARG or $HOME."
  (or
   (and (s-equals? arg "EMACSD") user-emacs-directory)
   (getenv (or arg "HOME"))
   (progn (message (format "[my-getenv] not found %s" arg))
          nil)))

(defun my-getenv-join (arg &rest path)
  "Get env ARG and join PATH."
  (let ((p (if (null path) ""
             (apply 'f-join path))))
    (f-join (my-getenv arg) p)))

(unbind-key "C-\\")
;; for prefix
(unbind-key "M-s w")
(unbind-key "M-m")
(unbind-key "C-]")
(unbind-key "C-x m")
(unbind-key "M-t")
(unbind-key "M-j")
(unbind-key "C-x f")

(add-to-list 'load-path (my-getenv-join "EMACSD" "site-lisp"))

(use-package my-bootstrap
  :demand t
  :straight (my-bootstrap :type built-in))

(my-macro-advice-add-debug straight-vc)

(use-package my-load-built-in
  :straight (my-load-built-in :type built-in))

(use-package my-theme
  :straight (my-theme :type built-in))

(use-package vterm ; libvterm libtool
  :bind
  ("M-s s" . vterm)
  :custom
  (vterm-keymap-exceptions
   '("C-@" ; set mark
     "C-c" ; prefix
     "C-x" ; prefix
     "C-u" ; universal argument
     "C-g" ; cancel
     "C-l" ; recenter
     "M-x" ; exec command
     "C-v" ; scroll
     "M-v" ; scroll
     "C-y" ; yank
     "M-y" ; yank
     "M-s" ; prefix
     "C-t" ; other window
     "M-j" ; prefix
     ))
  (vterm-max-scrollback 10000)
  (vterm-always-compile-module t))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-line-or-code)
  ("C-e" . mwim-end-of-line-or-code))

(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2)
  :custom
  (comment-dwim-2-inline-comment-behavior 'reindent-comment))

(use-package which-key
  :demand t
  :diminish (which-key-mode . "")
  :custom
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-setup-side-window-right)
  (which-key-mode t))

(use-package rainbow-mode
  :diminish (rainbow-mode . "")
  :hook
  ((prog-mode text-mode) . rainbow-mode)
  :custom
  (rainbow-ansi-colors t)
  (rainbow-html-colors t)
  (rainbow-x-colors t)
  (rainbow-latex-colors t))

(use-package rainbow-delimiters
  :hook
  ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package reformatter
  :bind
  ("M-s f" . my-reformatter-format)
  :config
  (require 'my-reformatter))

(use-package treemacs
  :bind
  (("M-t" . treemacs-select-window)
   :map treemacs-mode-map
   ("M-t" . delete-window) ; delete treemacs window
   ([mouse-1] . treemacs-single-click-expand-action))
  :custom
  (treemacs-no-png-images t) ; no icon
  (treemacs-indentation 1)
  (treemacs-indentation-string "|")
  (treemacs-silent-refresh t)
  (treemacs-show-cursor t)
  (treemacs-silent-filewatch t)
  (treemacs-file-event-delay 500)
  (treemacs-file-follow-delay 0.2)
  :config
  (defun my--revert-buffer--treemacs-refersh-after-advice ()
    (call-interactively 'treemacs-refresh))
  (advice-add #'my--revert-buffer :after #'my--revert-buffer--treemacs-refersh-after-advice)

  (defun my-treemacs-delete-other-window-predicate (window)
    "Prevent WINDOW from `delete-other-windows'."
    (not (string-match-p "Treemacs-Scoped-Buffer" (buffer-name (window-buffer window)))))
  (my-misc-delete-window-predicates-add 'my-treemacs-delete-other-window-predicate)

  (defun my-treemacs-other-window-predicate (window)
    "Prevent WINDOW from `other-window'."
    (not (string-match-p "Treemacs-Scoped-Buffer" (buffer-name (window-buffer window)))))
  (my-misc-other-window-predicates-add 'my-treemacs-other-window-predicate)

  (treemacs-git-commit-diff-mode 1)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-hide-gitignored-files-mode nil)
  (treemacs-fringe-indicator-mode 'always))

(use-package posframe)

;; search with romaji
(use-package migemo
  :custom
  (migemo-dictionary (my-getenv "MIGEMO_DICT"))
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  (migemo-init))

(use-package prescient
  :config
  (prescient-persist-mode t)
  :custom
  (prescient-save-file (my-getenv-join "EMACSD" "prescient-save.el"))
  (prescient-filter-method '(literal regexp initialism fuzzy)))

(use-package company-prescient
  :after (company prescient)
  :config
  (company-prescient-mode t)
  :custom
  (company-prescient-sort-length-enable nil))

(use-package vertico-prescient
  :after (vertico prescient)
  :config
  (vertico-prescient-mode t)
  :custom
  (vertico-prescient-enable-filtering nil))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-count 30)
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-scroll-margin 0))

(use-package consult
  :demand t
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (defvar my-consult--git-find-history
    nil
    "History of `my-consult-git-find'.")
  (defun my-consult--git-find (prompt builder initial)
    "Run git-find.sh in current directory.

The function returns the selected file.
The filename at point is added to the future history.

BUILDER is the command line builder function.
PROMPT is the prompt.
INITIAL is initial input."
    (consult--read
     (consult--process-collection builder
       :highlight t :file-handler t)
     :prompt prompt
     :sort nil
     :require-match t
     :initial initial
     :add-history (thing-at-point 'filename)
     :category 'file
     :history '(:input my-consult--git-find-history)))
  (defvar my-consult--git-find-args
    (my-getenv-join "DOTFILES_ROOT" "bin" "git-find.sh")
    "Command line arguments for find.")
  (defun my-consult--git-find-make-builder (paths)
    "Build find command line, finding across PATHS."
    (let* ((cmd (seq-mapcat (lambda (x)
                              (if (equal x ".") paths (list x)))
                            (consult--build-args my-consult--git-find-args))))
      (lambda (input)
        (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                     (`(,re . ,hl) (consult--compile-regexp arg 'basic t)))
          (when re
            (cons (append cmd re opts) hl))))))
  (defun my-consult-git-find (&optional dir initial)
    "Search for files with `git-find.sh' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments."
    (interactive "P")
    (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Git-find" dir))
                 (default-directory dir)
                 (builder (my-consult--git-find-make-builder paths)))
      (find-file (my-consult--git-find prompt builder initial))))

  (my-macro-region-or-at-point-direct my-consult-git-find)
  (my-macro-region-or-at-point-direct consult-line)
  (my-macro-region-or-at-point-direct consult-line-multi)
  (defun my-consult-git-grep (&optional initial dir)
    (interactive "P")
    (consult-git-grep dir initial))
  (my-macro-region-or-at-point-direct my-consult-git-grep)
  (defun my-consult-fd (&optional initial dir)
    (interactive "P")
    (consult-fd dir initial))
  (my-macro-region-or-at-point-direct my-consult-fd)
  (bind-key* "C-x o" 'consult-line-region-or-at-point)
  (bind-key* "C-x C-o" 'consult-line-multi-region-or-at-point)
  (bind-key* "C-x g g" 'my-consult-git-grep-region-or-at-point)
  (bind-key* "C-x C-g C-g" 'my-consult-git-find-region-or-at-point)
  (bind-key* "C-x f" 'my-consult-fd-region-or-at-point)
  (bind-key* "C-x r g" 'consult-ripgrep)
  :bind
  (("C-x g f" . consult-project-buffer) ; find file in project
   ("M-g X" . consult-register)
   ("M-g M-x" . consult-register-store)
   ("M-g x" . consult-register-load)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-bookmark)
   ("M-g k" . consult-mark)
   ("M-g K" . consult-global-mark)
   ("M-g M-g" . consult-goto-line) ; goto-line
   ("M-z" . consult-complex-command) ; repeat-complex-command
   ("M-y" . consult-yank-pop) ; yank
   ("C-x b" . consult-buffer) ; switch-to-buffer
   ("M-g i" . consult-imenu)
   ("M-g M-i" . consult-imenu-multi)
   ("M-s u" . consult-flycheck)
   :map isearch-mode-map
   ("C-l" . consult-isearch-history)
   ("C-o" . consult-line)
   ("C-O" . consult-line-multi)
   :map minibuffer-local-map
   ("C-o" . consult-history)))

(use-package consult-dir
  :ensure t
  :after (consult vertico)
  :bind
  (:map vertico-map
        ("C-d" . consult-dir)
        ("C-j" . consult-dir-jump-file)))

(use-package affe
  :after (consult orderless)
  :custom
  (affe-highlight-function 'orderless-highlight-matches)
  (affe-regexp-function 'orderless-pattern-compiler)
  (affe-find-command "git ls-files --full-name"))

(use-package consult-ghq
  :after consult
  :bind
  ("C-x c f" . consult-ghq-find)
  ("C-x c g" . consult-ghq-grep))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("M-0" . embark-act)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("M-0" . embark-act)
   :map embark-collect-mode-map
   ("M-0" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package bm
  :demand t
  :init
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook (lambda ()
                               (bm-buffer-save-all)
                               (bm-repository-save)))
  :bind
  ("M-s m" . bm-toggle)
  :custom
  (bm-buffer-persistence t)
  (bm-repository-file (my-getenv-join "EMACSD" ".bm"))
  (bm-cycle-all-buffers t)
  (bm-restore-repository-on-load t))

(use-package projectile
  :demand t
  :commands my-project-p
  :diminish (projectile-mode . "P")
  :bind
  ("C-c p f" . projectile-find-file)
  :config
  (defun my-project-p ()
    "Check if current directory is a project."
    (and (buffer-file-name)
         (s-starts-with-p (projectile-project-root) (file-truename (buffer-file-name)))))
  (projectile-mode)
  (setq projectile-mode-line-lighter "")
  (setq projectile-mode-line "")
  (defun my-advice-project-root (orig-func &rest args)
    (let ((r (apply orig-func args)))
      (if (not (equal r (format "%s/" (my-getenv-join "HOME"))))
          r
        (format "%s/" (my-getenv-join "EMACSD")))))
  (advice-add 'projectile-project-root :around 'my-advice-project-root)
  :custom
  (projectile-use-git-grep t)
  (projectile-dynamic-mode-line nil))

;; key bindings when region activated
(use-package selected
  :demand t
  :diminish (selected-minor-mode . "")
  :commands (shell-command-on-region-and-insert
             shell-command-on-region-and-replace)
  :bind
  (:map selected-keymap
        ("@" . rectangle-mark-mode)
        ("c" . count-words-region)
        ("Q" . query-replace)
        ("1" . sort-lines)
        ("2" . reverse-region)
        ("i" . indent-rigidly)
        ("b" . shell-command-on-region)
        ("C-b" . shell-command-on-region-and-insert)
        ("B" . shell-command-on-region-and-replace))
  :config
  (defun shell-command-on-region-and-insert (command)
    "Execute COMMAND with specified region and insert result into current buffer."
    (interactive (list (read-string "Shell command on region*: ")))
    (when (use-region-p)
      (shell-command-on-region (region-beginning) (region-end) command t)))
  (defun shell-command-on-region-and-replace (command)
    "Execute COMMAND with specified region and replace region with result."
    (interactive (list (read-string "Shell command on region**: ")))
    (when (use-region-p)
      (shell-command-on-region (region-beginning) (region-end) command t t)))
  (setq selected-minor-mode-override t)
  (selected-global-mode 1))

(use-package my-pipenv
  :demand t
  :after consult
  :straight (my-pipenv :type built-in)
  :config
  (defun my-pipenv-consult-find (module)
    (consult-find (my-pipenv-get-path module)))
  (my-macro-region-or-at-point my-pipenv-consult-find "Module: ")
  (bind-key "M-s 3" 'my-pipenv-consult-find-region-or-at-point)
  :custom
  (my-pipenv-get-path-command (my-getenv-join "DOTFILES_ROOT" "bin/pipenv_get_path.sh")))

(use-package deadgrep
  :demand t
  :commands deadgrep
  :config
  (add-to-list 'deadgrep-project-root-overrides `("~/" . ,(concat (my-getenv-join "DOTFILES_ROOT") "/"))) ; deny searching at the home directory
  (my-macro-thyristor my-deadgrep-specify-path)
  (my-deadgrep-specify-path-thyristor-set nil)
  (defvar my-deadgrep-specify-path--path (concat (my-getenv-join "DOTFILES_ROOT") "/"))
  (defun my-advice-deadgrep--lookup-override (orig-func &rest args)
    (if my-deadgrep-specify-path-thyristor-flag
        (progn
          (my-deadgrep-specify-path-thyristor-set nil)
          my-deadgrep-specify-path--path)
      (apply orig-func args)))
  (advice-add 'deadgrep--lookup-override :around 'my-advice-deadgrep--lookup-override)
  (defun my-deadgrep-with-path (path)
    "Do `deadgrep' on `path'."
    (interactive "fPath: ")
    (setq my-deadgrep-specify-path--path path)
    (my-deadgrep-specify-path-thyristor-set t)
    (call-interactively 'deadgrep))
  (my-macro-thyristor my-deadgrep-set-search-hidden-files)
  (my-deadgrep-set-search-hidden-files-thyristor-set nil)
  (defun my-advice-deadgrep--arguments (orig-func &rest args)
    (let ((result (apply orig-func args)))
      (when my-deadgrep-set-search-hidden-files-thyristor-flag
        (push "--hidden" result)
        (my-deadgrep-set-search-hidden-files-thyristor-set nil))
      result))
  (advice-add 'deadgrep--arguments :around 'my-advice-deadgrep--arguments)
  (defun my-deadgrep-hidden (term)
    "Do `deadgrep' with rg --hidden."
    (interactive "s")
    (my-deadgrep-set-search-hidden-files-thyristor-set t)
    (deadgrep term))
  (my-macro-region-or-at-point deadgrep "[deadgrep] ")
  (my-macro-region-or-at-point my-deadgrep-hidden "[deadgrep-hidden] ")
  (bind-key "C-x a o" 'deadgrep-region-or-at-point)
  (bind-key "C-x a a" 'my-deadgrep-hidden-region-or-at-point)
  :bind
  ("C-x a p" . my-deadgrep-with-path))

(use-package dumb-jump
  :demand t
  :after (deadgrep projectile)
  :bind
  ("M-g ?" . xref-find-references)
  ("M-g b" . xref-go-back)
  ("M-g h" . xref-find-apropos)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (my-macro-fallback-interactively my-project-p xref-find-definitions deadgrep)
  (bind-key "M-g g" 'xref-find-definitions-fallback-to-deadgrep)
  :custom
  (dumb-jump-default-project (my-getenv-join "DOTFILES_ROOT"))
  (dumb-jump-selector 'helm)
  (dumb-jump-force-searcher 'rg))

(use-package goto-chg
  :demand t
  :after projectile
  :config
  (bind-key "M-," 'goto-last-change)
  (bind-key "M-." 'goto-last-change-reverse))

;; completions
(use-package company
  :demand t
  :diminish (company-mode . "")
  :bind
  (("C-^" . company-complete)
   ("TAB" . indent-for-tab-command)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("TAB" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :custom-face
  (company-tooltip ((t (:foreground "Black" :background "DimGray"))))
  (company-tooltip-common ((t (:foreground "Yellow" :background "DimGray"))))
  (company-tooltip-common-selection ((t (:foreground "Yellow" :background "SteelBlue"))))
  (company-tooltip-selection ((t (:foreground "Yellow" :background "SteelBlue"))))
  (company-preview-common ((t (:foreground "DimGray" :background "unspecified" :underline t))))
  (company-scrollbar-fg ((t (:background "Orange"))))
  (company-scrollbar-bg ((t (:background "Gray"))))
  :custom
  (company-tooltip-maximum-width 50)
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.7)
  (company-require-match nil)
  (company-transformers '(company-sort-by-backend-importance))
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")
  (company-backends '(company-bbdb
                      company-eclim
                      company-semantic
                      company-clang
                      company-xcode
                      company-cmake
                      (company-capf company-dabbrev) ; capf needs yas-minor-mode
                      company-files
                      (company-dabbrev-code company-gtags company-etags company-keywords)
                      company-oddmuse))
  :config
  (global-company-mode))

(use-package company-quickhelp
  :hook
  (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-max-lines 20))

(use-package git-complete
  :straight (git-complete :host github :repo "zk-phi/git-complete")
  :bind
  ("M-s ;" . git-complete)
  :custom
  (git-complete-enable-autopair t)
  (git-complete-ignore-case t))

(use-package avy-migemo
  :demand t
  :bind
  (("C-j" . avy-migemo-goto-char-timer)
   :map isearch-mode-map
   ("C-j" . avy-migemo-isearch))
  :config
  (avy-migemo-mode t))

(use-package expand-region
  :after selected
  :bind
  (:map selected-keymap
        ("j" . er/contract-region)
        ("k" . er/expand-region))
  :config
  (transient-mark-mode t))

(use-package multiple-cursors
  :config
  (unbind-key "M-^")
  (bind-key "M-^ ." 'mc/edit-lines)
  (bind-key "M-^ >" 'mc/mark-next-like-this)
  (bind-key "M-^ <" 'mc/mark-previous-like-this))

;; search assist
(use-package anzu
  :custom
  (anzu-deactivate-region t)
  (anzu-mode-lighter "")
  (anzu-search-threshold 100)
  :config
  (global-anzu-mode +1))

(use-package ez-query-replace
  :bind
  ("M-%" . ez-query-replace))

(use-package my-paren
  :straight (my-paren :type built-in)
  :bind
  ("M-] ]" . my-paren-surround-region)
  ("M-] ." . my-paren-remove-parens)
  ("M-] k" . my-paren-kill-sexp))

(use-package smartparens
  :diminish (smartparens-mode . "")
  :demand t
  :config
  (ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
  (ad-activate 'delete-backward-char)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil) ; disable right ' completion
  (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil) ; disable right ` completion
  (sp-local-pair '(plantuml-mode) "'" "'" :actions nil) ; disable right ' completion
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map
        ("M-g M-b" . markdown-follow-thing-at-point)
        ("C-c +" . markdown-promote)
        ("C-c -" . markdown-demote)
        ("M-n" . scroll-util-scroll-up-relationally)
        ("M-p" . scroll-util-scroll-down-relationally)
   :map gfm-mode-map
        ("M-g M-b" . markdown-follow-thing-at-point)
        ("C-c +" . markdown-promote)
        ("C-c -" . markdown-demote)
        ("M-n" . scroll-util-scroll-up-relationally)
        ("M-p" . scroll-util-scroll-down-relationally))
  :config
  (add-hook 'gfm-mode-hook (lambda () (goto-address-mode -1)))
  (add-hook 'markdown-mode-hook (lambda () (goto-address-mode -1)))
  (defun markdown-mode-before-save-hook ()
    "Disable `delete-trailing-whitespace' if `major-mode' is `markdown-mode' or `gfm-mode'."
    (delete-trailing-whitespace-thyristor-set (not (memq major-mode '(markdown-mode gfm-mode)))))
  (add-hook 'before-save-hook 'markdown-mode-before-save-hook)
  :custom
  (markdown-command '("pandoc" "--from=markdown" "--to=html5" "--standalone=true" "--ascii=false"))
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-command-needs-filename t)
  (markdown-content-type "application/xhtml+xml")
  (markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))
  (markdown-xhtml-header-content "
<script>
document.addEventListener('DOMContentLoaded', (event) => {
  document.body.classList.add('markdown-body');
});
</script>") ; github-markdown.min.css is applied to .markdown-body
  (markdown-fontify-code-blocks-natively t))

(use-package flyspell-correct
  :bind
  (:map flyspell-mode-map
        ("M-s :" . flyspell-correct-wrapper))
  :after flyspell)

(use-package flyspell-correct-popup
  :after flyspell)

;; spell check
(use-package flyspell
  :diminish (flyspell-mode . "Fs")
  :hook
  ((prog-mode . flyspell-prog-mode)
   ((text-mode markdown-mode gfm-mode) . flyspell-mode))
  :config
  ; for backward-forward
  (unbind-key "C-," flyspell-mode-map)
  (unbind-key "C-." flyspell-mode-map))

(use-package cc-mode
  :mode
  ("\\.h\\'" . c++-mode)
  :hook
  (c-mode-common . (lambda ()
                     (setq c-default-style "k&r")
                     (setq indent-tabs-mode nil)
                     (setq c-basic-offset 2))))

(use-package add-node-modules-path
  :hook
  ((js-mode . add-node-modules-path)
   (js2-mode . add-node-modules-path)
   (web-mode . add-node-modules-path)))

(use-package web-mode
  :commands (web-mode web-mode-indent)
  :after flycheck
  :mode
  (("\\.(dj|p)?html?\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.gsp\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.xml\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.vuex?\\'" . web-mode)
   ("\\.twig\\'" . web-mode))
  :hook
  (web-mode . (lambda ()
                (setq web-mode-attr-indent-offset nil)
                (setq indent-tabs-mode nil)))
  :config
  (defun web-mode-indent (n)
    "Set variables for `web-mode' to set indent tab width."
      (interactive "nTab-width: ")
      (setq web-mode-markup-indent-offset n
            web-mode-css-indent-offset n
            web-mode-style-padding n
            web-mode-code-indent-offset n
            web-mode-script-padding n
            web-mode-javascript-indentation n
            web-mode-block-padding n))
  (setq web-mode-enable-auto-pairing nil ; due to smartparens
        web-mode-enable-auto-quoting nil
        web-mode-tag-auto-close-style t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)
  (web-mode-indent 2)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))

(use-package go-mode
  :custom
  (gofmt-command "goimports")
  :config
  (my-macro-thyristor gofmt-before-save)
  (gofmt-before-save-thyristor-set nil)
  (add-hook 'before-save-hook 'gofmt-before-save-thyristor))

(use-package my-flycheck-golangci-lint
  :straight (my-flycheck-golangci-lint :type built-in)
  :after (go-mode flycheck)
  :hook (go-mode . my-flycheck-golangci-lint-setup))

(use-package go-playground
  :init
  (defconst my-go-playground-sh
    (my-getenv-join "DOTFILES_ROOT" "bin" "go-playground.sh"))
  :bind
  (:map go-playground-mode-map
        ("C-c C-c" . my-go-playground-run))
  :config
  (defun my-go-playground-run ()
    "Run snippet.go."
    (interactive)
    (compile (format "%s run" my-go-playground-sh)))
  :custom
  (go-playground-basedir (f-join (my-getenv "GHQ_ROOT") "github.com" (my-getenv "GIT_USER") "go-playground"))
  (go-playground-init-command (format "%s init" my-go-playground-sh)))

(use-package tern
  :custom
  (tern-command '("tern" "--no-port-file"))) ; no .tern-port

(use-package js2-mode
  :hook
  (js-mode . (lambda ()
               (make-local-variable 'js-indent-level)
               (setq js-indent-level 2
                     indent-tabs-mode nil
                     c-basic-offset 2
                     js2-basic-offset 2
                     tab-width 2)))
  :mode
  (("\\.m?js\\'" . js2-mode)
   ("\\.jsx$" . web-mode)
   ("\\.qml$" . js2-mode)))

(use-package typescript-mode
  :ensure t
  :hook
  (typescript-mode . (lambda ()
                       (setq typescript-indent-level 2
                             flycheck-check-syntax-automatically '(save mode-enabled))))
  :mode
  ("\\.ts\\'" . typescript-mode)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook
  (python-mode . (lambda ()
                   (setq indent-tabs-mode nil)
                   (setq python-indent 4)))
  :init
  (add-to-list 'exec-path (my-getenv-join "PYENV_ROOT" "shims"))
  :custom
  (python-shell-interpreter (my-getenv-join "PYENV_ROOT" "shims" "python"))
  :config
  (setq py-python-command (my-getenv-join "PYENV_ROOT" "shims" "python")))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package flymake-ruff
  :ensure t
  :straight (flymake-ruff :type git :host github :repo "erickgnavar/flymake-ruff")
  :hook (python-mode . flymake-ruff-load))

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :custom
  (ruby-deep-indent-paren-style nil)
  (ruby-indent-tabs-mode nil))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))

(use-package php-mode
  :hook
  (php-mode . (lambda ()
                (setq c-basic-offset 4)))
  :mode ("\\.php\\'" . php-mode)
  :config
  (php-enable-default-coding-style)
  (subword-mode 1))

(use-package flycheck-phpstan
  :after (php-mode flycheck))

(use-package scala-mode
  :mode "^\w+\\.s\\(cala\\|bt\\)$")

(use-package ascii
  :bind
  (("M-s M-s M-a" . ascii-on)
   ("M-s M-s a" . ascii-off)))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :bind
  (:map json-mode-map
        ("M-s f" . json-mode-beautify))
  :config
  (unbind-key "C-c :" json-mode-map)
  (unbind-key "C-c C-r" json-mode-map))

(use-package yaml-mode
  :mode
  (("\\.yaml\\'" . yaml-mode)
   ("\\.yml\\'" . yaml-mode)
   ("\\.dig\\'" . yaml-mode))
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent)))

(use-package dockerfile-mode
  :mode
  ("Dockerfile" . dockerfile-mode)
  :config
  (add-hook 'dockerfile-mode-hook
            (lambda () (electric-indent-local-mode -1))))

(use-package rust-mode
  :hook
  (rust-mode . (lambda ()
                 (setq indent-tabs-mode nil)
                 (smartparens-mode 1)
                 (global-display-line-numbers-mode 1)))
  :custom
  (rust-rustfmt-switches '("--config" "edition=2024"))
  (rust-format-on-save t))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-playground
  :custom
  (rust-playground-basedir (f-join (my-getenv "GHQ_ROOT") "github.com" (my-getenv "GIT_USER") "rust-playground")))

(use-package flycheck-haskell
  :after (haskell-mode flycheck)
  :hook
  (haskell-mode . flycheck-haskell-setup))

(use-package haskell-mode
  :mode
  ("\\.hs$" . haskell-mode))

(defalias 'perl-mode 'cperl-mode)

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package plantuml-mode
  :init
  (defun my-plantuml-local-hook ()
    (flycheck-mode nil))
  (defun add-plantuml-local-hook ()
    (add-hook 'before-save-hook 'my-plantuml-local-hook nil t))
  (add-hook 'plantuml-mode 'add-plantuml-local-hook)
  :config
  (setq plantuml-output-type "png")
  :custom
  (plantuml-indent-level 2)
  (plantuml-executable-args '("-headless" "-theme" "toy"))
  (plantuml-default-exec-mode 'executable)
  (plantuml-executable-path "plantuml"))

(use-package lua-mode)

;; syntax checkers
(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :commands flycheck-pos-tip-mode)

(use-package flycheck-popup-tip
  :after flycheck
  :commands flycheck-popup-tip-mode)

(use-package flycheck
  :demand t
  :commands
  (flycheck-mode
   flycheck-mode-thyristor-2n
   my-flycheck-mode-thyristor-switch)
  :hook
  ((go-mode
    dart-mode
    cperl-mode
    scala-mode
    python-mode
    c-mode
    sh-mode
    text-mode
    markdown-mode
    gfm-mode
    emacs-lisp-mode
    dockerfile-mode
    protobuf-mode
    haskell-mode
    php-mode
    js2-mode) . flycheck-mode-thyristor-2n)
  :bind
  (("M-s M-s M-f" . flycheck-mode))
  :custom
  (flycheck-idle-change-delay 5)
  (flycheck-display-errors-delay 1)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
  :config
  (defun my-flycheck-mode-thyristor-switch ()
    "Call `flycheck-mode-thyristor-toggle' and enable/disable `flycheck-mode'."
    (interactive)
    (flycheck-mode-thyristor-toggle)
    (flycheck-mode (if flycheck-mode-thyristor-flag 1 0)))
  (flycheck-define-checker textlint
    "Linter for text."
    :modes (text-mode markdown-mode gfm-mode)
    :command ("textlinter" source) ; $DOTFILES_ROOT/bin/textlint.sh
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ":["
              (id (one-or-more (not (any "]")))) "]"
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end)))
  (my-macro-thyristor flycheck-mode)
  (if (display-graphic-p)
      (flycheck-pos-tip-mode)
    (flycheck-popup-tip-mode)))

;; syntax highlighting
(use-package tree-sitter-langs)
(use-package tree-sitter
  :diminish  (tree-sitter-mode . "")
  :after tree-sitter-langs
  :demand t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(typescript-tsx-mode . tsx)))

(use-package eglot
  :hook
  (((python-mode
     ruby-mode
     go-mode
     rust-mode
     c-mode
     c++-mode) . eglot-ensure)
   (eglot-managed-mode . (lambda () (flymake-mode 0)))
   (eglot-managed-mode . my-eglot-before-save-hook)
   (eglot-managed-mode . eglot-inlay-hints-mode))
  :bind
  (("M-s M-s M-e" . eglot)
   :map eglot-mode-map
   ("C-x p m r" . eglot-reconnect)
   ("C-x p f" . eglot-format-buffer)
   ("C-x p a" . eglot-code-actions)
   ("C-x p r" . eglot-rename)
   ("M-s M-s M-e" . eglot-shutdown))
  :init
  (defun my-eglot-require-with-check-around-advice (orig-func &rest args)
    "If the 3rd argument is not specified, specify t.
Workaround for:
internal-macroexpand-for-load: Eager macro-expansion failure: (error \"Feature provided by other file: project\")
when (eglot)."
    (message "DEBUG my-eglot-require-with-check-around-advice: %S" args)
    (let* ((arg-len (length args))
           (new-args (cond
                      ((eq arg-len 1) (append args (list nil t)))
                      ((eq arg-len 2) (append args (list t)))
                      (t args))))
      (apply orig-func new-args)))
  (advice-add 'require-with-check :around #'my-eglot-require-with-check-around-advice)
  :config
  (my-macro-ring-hook "my-eglot" '(my-eglot-imports-and-format
                                   my-eglot-noop
                                   my-eglot-format
                                   my-eglot-imports))
  (defun my-eglot-ring-echo (cur next)
    (message "[my-eglot] change before save hook to %s" next))
  (add-to-list 'my-eglot-ring-hook 'my-eglot-ring-echo)
  (defun my-eglot-format-buffer ()
    (when (or (eq (my-eglot-ring-hook-get-state) 'my-eglot-format)
              (eq (my-eglot-ring-hook-get-state) 'my-eglot-imports-and-format))
      (call-interactively 'eglot-format-buffer)))
  (defun my-eglot-code-action-organize-imports ()
    (when (or (eq (my-eglot-ring-hook-get-state) 'my-eglot-imports)
              (eq (my-eglot-ring-hook-get-state) 'my-eglot-imports-and-format))
      (call-interactively 'eglot-code-action-organize-imports)))
  (defun my-eglot-format-and-imports ()
    (my-eglot-format-buffer)
    (my-eglot-code-action-organize-imports))
  (defun my-eglot-before-save-hook ()
    (add-hook 'before-save-hook #'my-eglot-format-and-imports))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls"
                                                   :initializationOptions
                                                   (:hints (:parameterNames t
                                                            :rangeVariableTypes t
                                                            :functionTypeParameters t
                                                            :assignVariableTypes t
                                                            :compositeLiteralFields t
                                                            :compositeLiteralTypes t
                                                            :constantValues t)))))
  (add-to-list 'eglot-server-programs '(ruby-mode . ("solargraph" "stdio")))
  :custom
  (eglot-autoreconnect nil)
  (eglot-connect-timeout 20)
  (eglot-extend-to-xref t)
  (eglot-send-changes-idle-time 5)
  (eglot-confirm-server-initiated-edits nil))

(use-package consult-eglot
  :ensure t
  :bind
  (:map eglot-mode-map
        ("C-x p o" . consult-eglot-symbols))
  :after (consult eglot))

(use-package yasnippet)
(use-package lsp-mode
  :hook
  (((typescript-mode
     typescript-tsx-mode
     ;; go-mode
     ;; ruby-mode
     ;; python-mode
     ;; c-mode
     ;; c++-mode
     php-mode
     svelte-mode
     ;; rust-mode
     css-mode
     terraform-mode
     html-mode) . lsp-deferred)
   (lsp-mode . lsp-lens-mode))
  :bind
  (("M-s M-s M-l" . lsp)
   :map lsp-mode-map
   ("M-s M-s M-l" . lsp-disconnect)
   ("C-c p w" . lsp-workspace-folders-add)
   ("M-s M-s l" . lsp-workspace-restart)
   ("C-c p C-w" . lsp-workspace-shutdown)
   ("C-c p m d" . lsp-describe-session)
   ("C-c p ." . lsp-describe-thing-at-point)
   ("C-c p a" . lsp-execute-code-action)
   ("C-c p r" . lsp-rename))
  :custom
  (lsp-disabled-clients '(tfls))
  (lsp-auto-guess-root t)
  (lsp-prefer-capf t)
  (lsp-prefer-flymake nil)
  (lsp-signature-auto-activate t)
  (lsp-print-performance nil)
  (lsp-log-io t)
  (lsp-trace nil)
  (lsp-restart 'ignore)
  (lsp-idle-delay 0.7)
  (lsp-file-watch-threshold 100)
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-enable-indentation nil)
  (lsp-enable-completion-at-point t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-text-document-color t)
  (lsp-enable-file-watchers t)
  (lsp-inlay-hint-enable t)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t)
  (lsp-diagnostic-package nil)
  (lsp-document-sync-method lsp--sync-incremental)
  (lsp-response-timeout 1.2)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints t)
  (lsp-terraform-enable-logging nil)
  (lsp-yaml-format-enable nil)
  (lsp-yaml-print-width 120)
  (lsp-yaml-prose-wrap 'never)
  :config
  (lsp-register-custom-settings
   '(("gopls.hints" ((assignVariableTypes . t)
                     (compositeLiteralFields . t)
                     (compositeLiteralTypes . t)
                     (constantValues . t)
                     (functionTypeParameters . t)
                     (parameterNames . t)
                     (rangeVariableTypes . t))))))

(use-package lsp-ui
  :commands
  (ladicle/toggle-lsp-ui-doc
   lsp-ui-peek-find-references
   lsp-ui-peek-find-definitions
   lsp-ui-peek-find-implementation)
  :bind
  (:map lsp-mode-map
        ("M-g ?" . lsp-ui-peek-find-references)
        ("M-g g" . lsp-ui-peek-find-definitions)
        ("C-c p i" . lsp-ui-peek-find-implementation)
        ("C-c p s" . lsp-ui-sideline-mode)
        ("C-c p d" . ladicle/toggle-lsp-ui-doc))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-delay 0.3)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-imenu-enable nil)
  :config
  (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1))))

(use-package dap-mode
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (require 'dap-dlv-go))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  :bind
  (:map lsp-mode-map
        ("C-c t e" . lsp-treemacs-errors-list)
        ("C-c t l" . lsp-treemacs-symbols)
        ("C-c t ?" . lsp-treemacs-references)
        ("C-c t i" . lsp-treemacs-implementations)
        ("C-c t c" . lsp-treemacs-call-hierarchy)
        ("C-c t t" . lsp-treemacs-type-hierarchy)))

(use-package dsvn
  :commands (svn-status svn-update)
  :config
  (require 'vc-svn)
  (setq svn-program (my-getenv-join "DOTFILES_ROOT" "bin" "dsvn.sh")))

(use-package restclient)

(use-package diff-hl
  :diminish
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package backward-forward
  :demand t
  :after (consult deadgrep with-editor)
  :bind
  ("C-," . backward-forward-previous-location)
  ("C-." . backward-forward-next-location)
  :config
  (defun my-backward-forward-clear-mark-ring ()
    (interactive)
    (setq backward-forward-mark-ring nil))
  (defun my-backward-forward-push-mark (orig-func &rest args)
    (let* ((before-buffer-name (buffer-name))
           (before-point (point))
           (result (apply orig-func args))
           (after-buffer-name (buffer-name))
           (after-point (point)))
      (message "DEBUG [my-backward-forward-push-mark] %s %d -> %s %d"
               before-buffer-name before-point
               after-buffer-name after-point)
      result))
  (cl-loop for x in '(deadgrep
                      find-file
                      project-find-file
                      project-find-regexp
                      consult-project-buffer
                      consult-buffer
                      consult-imenu
                      consult-goto-line
                      consult-ghq-find
                      consult-ghq-grep
                      consult-line
                      consult-line-multi
                      consult-git-grep
                      beginning-of-buffer
                      end-of-buffer
                      isearch-forward
                      isearch-backward
                      xref-go-back
                      xref-find-references
                      xref-find-definitions)
           do (advice-add x :around 'my-backward-forward-push-mark))
  (backward-forward-mode t))

(use-package indent-guide
  :custom
  (indent-guide-delay 0.4)
  :config
  (indent-guide-global-mode))

(use-package command-log
  :straight (command-log :host github :repo "berquerant/emacs-command-log")
  :custom
  (command-log-histfile (my-getenv "EMACS_HISTFILE"))
  :config
  (command-log-setup))

(use-package thread-buffer
  :straight (thread-buffer :host github :repo "berquerant/emacs-thread-buffer"))

(use-package thread-buffer-chat
  :straight (thread-buffer-chat :host github :repo "berquerant/emacs-thread-buffer-chat"))

(use-package my-ai
  :straight (my-ai :type built-in))

(use-package my-man
  :straight (my-man :type built-in)
  :config
  (my-macro-region-or-at-point my-man-hman "hamn> ")
  (bind-key "M-s M-s h" 'my-man-hman-region-or-at-point))

(use-package message-routing
  :demand t
  :straight (message-routing :host github :repo "berquerant/emacs-message-routing")
  :custom
  (message-routing-routes '(("^LSP :: Error" . "*routed-lsp-error*")
                            ("^DEBUG" . "*routed-debug-log*")
                            ("^my-popup-tip" . "*my-tips*")
                            ("^my-pos-tip" . "*my-tips*")
                            ("^my-straight" . "*my-straight*")
                            ("^my-rpath" . "*my-rpath*")
                            ("^my-macro-advice-add-debug" . "*my-macro-advice-add-debug*")))
  :config
  (defun my-popup-tip-message-advice (orig-func &rest args)
    (message "my-popup-tip: %s" (nth 0 args))
    (apply orig-func args))
  (advice-add 'popup-tip :around 'my-popup-tip-message-advice)
  (defun my-pos-tip-show-message-advice (orig-func &rest args)
    (message "my-pos-tip: %s" (nth 0 args))
    (apply orig-func args))
  (advice-add 'pos-tip-show :around 'my-pos-tip-show-message-advice)
  (message-routing-setup))

(use-package my-straight
  :straight (my-straight :type built-in)
  :custom
  (my-straight-profile-path my-straight-profile)
  (my-straight-dir-path my-straight-directory))

(use-package my-open-file
  :straight (my-open-file :type built-in)
  :bind
  ("M-g f" . my-open-file-find)
  :custom
  (my-open-file-target (my-getenv "EMACS_OPEN_FILE_TARGET")))

(use-package my-external
  :straight (my-external :type built-in))

(message "init.el loaded.")
;;; init.el ends here
