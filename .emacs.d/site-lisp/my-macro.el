;;; my-macro.el --- my macros -*- lexical-binding: t -*-

;;; Commentary:

;; misc macros

;;; Code:

(require 's)

(defmacro my-macro-do-once (f)
  "Make lambda that invokes F, re-invocation of F is denied if you invoke F through the lambda."
  (let ((flag (format "%s-once-flag" (symbol-name f))))
    `(lambda ()
       (when (not (boundp (intern ,flag)))
         (setq ,(intern flag) t)
         (,f)))))

(defmacro my-macro-toggle (x)
  "Toggle X as boolean."
  `(setq ,(read (symbol-name x))
         (equal ,(read (symbol-name x)) nil)))

(defmacro my-macro-defun-toggle (x)
  "Define function toggle X."
  `(defun ,(read (format "toggle-%s" (symbol-name x)))
       ()
     ,(format "Toggle `%s'." (symbol-name x))
     (interactive)
     (my-macro-toggle ,(read (symbol-name x)))))

(defmacro my-macro-thyristor (f)
  "Define some functions and variables like F-thyristor-Y.

F-thyristor-flag is t initially.
F-thyristor-toggle toggles F-thyristor-flag.
F-thyristor-set sets boolean value to F-thyristor-flag.
Invoke F-thyristor then invoke F if F-thyristor-flag.
Invoke F-thyristor-2 then invoke (F F-thyristor-flag).
Invoke F-thyristor-2n then invoke (F (if F-thyristor-flag 1 0))."
  (let ((flag (format "%s-thyristor-flag" (symbol-name f)))
        (toggle (format "%s-thyristor-toggle" (symbol-name f)))
        (fset (format "%s-thyristor-set" (symbol-name f)))
        (thyristor (format "%s-thyristor" (symbol-name f)))
        (thyristor-2 (format "%s-thyristor-2" (symbol-name f)))
        (thyristor-2n (format "%s-thyristor-2n" (symbol-name f)))
        (doc-string (format "Generated by `my-macro-thyristor' from `%s'." (symbol-name f))))
    `(progn
       (defvar ,(read flag)
         t
         ,doc-string)
       (defun ,(read toggle)
           ()
         ,doc-string
         (interactive)
         (my-macro-toggle ,(read flag)))
       (defun ,(read fset)
           (&optional arg)
         ,doc-string
         (interactive)
         (setq ,(read flag) arg))
       (defun ,(read thyristor)
           ()
         ,doc-string
         (interactive)
         (when ,(read flag)
           (,f)))
       (defun ,(read thyristor-2)
           ()
         ,doc-string
         (interactive)
         (,f ,(read flag)))
       (defun ,(read thyristor-2n)
           ()
         ,doc-string
         (interactive)
         (,f (if ,(read flag) 1 0))))))

(defmacro my-macro-fallback (p first next)
  "Define a function that call FIRST if P else NEXT."
  (let ((f (format "%s-fallback-to-%s" (symbol-name first) (symbol-name next)))
        (doc-string (format "Generated by `my-macro-fallback' from `%s', `%s' and `%s'."
                            (symbol-name p) (symbol-name first) (symbol-name next))))
    `(progn
       (defun ,(read f)
           ()
         ,doc-string
         (interactive)
         (if (,p) (,first) (,next))))))

(defmacro my-macro-fallback-interactively (p first next)
  "Define a function that call interactively FIRST if P else NEXT."
  (let ((f (format "%s-fallback-to-%s" (symbol-name first) (symbol-name next)))
        (doc-string (format "Generated by `my-macro-fallback-interactively' from `%s', `%s' and `%s'."
                            (symbol-name p) (symbol-name first) (symbol-name next))))
    `(progn
       (defun ,(read f)
           ()
         ,doc-string
         (interactive)
         (if (,p) (call-interactively ',first) (call-interactively ',next))))))

(defmacro my-macro-state-hook (target-func target-state default-state)
  "Define a function that can invoke TARGET-FUNC with state.

You can define a function that invokes TARGET-FUNC
after assigning specified state to TARGET-STATE
by TARGET-FUNC-state-hook-generator macro,
it's name will be TARGET-FUNC-state-hook-STATE.
TARGET-FUNC-state-hook-STATE resets TARGET-STATE to DEFAULT-STATE finally."
  (let ((local-state (format "%s-state-hook-state" (symbol-name target-func)))
        (local-default-state (format "%s-state-hook-default-state" (symbol-name target-func)))
        (advice (format "%s-state-hook-advice" (symbol-name target-func)))
        (generator (format "%s-state-hook-generator" (symbol-name target-func)))
        (doc-string (format "Generated by `my-macro-state-hook' with `%s'." (symbol-name target-func))))
    `(progn
       (defconst ,(read local-default-state)
         ',default-state
         ,doc-string)
       (defvar ,(read local-state)
         ,(read local-default-state)
         ,doc-string)
       (defun ,(read advice)
           (orig-func &rest args)
         ,doc-string
         (setq ,target-state ,(read local-state))
         (apply orig-func args)
         (setq ,(read local-state) ,(read local-default-state)))
       (advice-add ',target-func :around ',(read advice))
       (defmacro ,(read generator)
           (state)
         ,doc-string
         (let ((f (format "%s-state-hook-%s" ,(symbol-name target-func) (if (symbolp state) (symbol-name state) state)))
               (s ,local-state)
               (d ,doc-string)
               (tf (symbol-name ',target-func)))
           `(progn
              (defun ,(read f)
                  ()
                ,d
                (interactive)
                (setq ,(read s) ',state)
                (call-interactively ',(read tf)))))))))

(defmacro my-macro-region-or-at-point (target-func prompt)
  "Generate a function that invokes TARGET-FUNC by region when region specified,
fallback to manual input with default input from `thing-at-point' (prompt is PROMPT).
Generated function is TARGET-FUNC-region-or-point."
  (let ((fname (format "%s-region-or-at-point" (symbol-name target-func)))
        (doc-string (format "Generated by `my-macro-region-or-at-point' with `%s'." (symbol-name target-func))))
    `(progn
       (defun ,(read fname)
           ()
           ,doc-string
           (interactive)
           (cond
            ((use-region-p) (let ((txt (buffer-substring (region-beginning) (region-end))))
                             (,target-func txt)))
            (t (let ((txt (read-string ,prompt (thing-at-point 'word))))
                 (,target-func txt))))))))

(defmacro my-macro-handle-file (handler path)
  "Generate HANDLER-FILE ('.' are deleted) function that apply HANDLER to PATH."
  (let* ((p (symbol-value path))
         (filename (file-name-nondirectory p))
         (fname (format "%s-%s"
                        (symbol-name handler)
                        (s-replace-all '(("." . "")) filename)))
         (doc-string (format "Generated by `my-macro-handle-file' with `%s' and %s."
                             (symbol-name handler) p)))
    `(progn
       (defun ,(read fname)
           ()
         ,doc-string
         (interactive)
         (,handler ,p)))))

(provide 'my-macro)
;;; my-macro.el ends here
