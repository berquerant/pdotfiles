;;; my-macro.el --- my macros -*- lexical-binding: t -*-

;;; Commentary:

;; misc macros

;;; Code:

(require 'cl-lib)
(require 's)
(require 'my-uuid)

(defmacro my-macro-advice-add-const (f &optional ret)
  "Disable F calls and return const RET."
  (let* ((fname (symbol-name f))
         (advice-name (format "my-macro-advice-add-const-aroound-%s" fname)))
    `(progn
       (defun ,(read advice-name) (orig-func &rest args)
         ,ret)
       (advice-add ',(read fname) :around ',(read advice-name)))))

(defmacro my-macro-advice-add-debug (f)
  "Add logs before and after the call of F."
  (let* ((fname (symbol-name f))
         (advice-name (format "my-macro-advice-add-debug-around-%s" fname)))
    `(progn
       (defun ,(read advice-name) (orig-func &rest args)
         (let ((uuid (my-uuid-gen)))
           (message "%s: [%s] start %s" ,advice-name uuid args)
           (let ((ret (apply orig-func args)))
             (message "%s: [%s] end %s" ,advice-name uuid ret)
             ret)))
       (advice-add ',(read fname) :around ',(read advice-name)))))

(defmacro my-macro-do-once (f)
  "Make lambda that invokes F, re-invocation of F is denied if you invoke F through the lambda."
  (let ((flag (format "%s-once-flag" (symbol-name f))))
    `(lambda ()
       (when (not (boundp (intern ,flag)))
         (setq ,(intern flag) t)
         (,f)))))

(defmacro my-macro-toggle (x)
  "Toggle X as boolean."
  `(progn
     (message "[my-macro-toggle] toggle %s into %s"
              ,(symbol-name x)
              (equal ,(read (symbol-name x)) nil))
     (setq ,(read (symbol-name x))
         (equal ,(read (symbol-name x)) nil))))

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

(defmacro my-macro-region-or-at-point-direct (target-func)
  "Generate a function that invokes TARGET-FUNC by region when region specified,
fallback to `thing-at-point' symbol or invoke TARGET-FUNC with no arguments.
Generated function is TARGET-FUNC-region-or-point."
  (let ((fname (format "%s-region-or-at-point" (symbol-name target-func)))
        (doc-string (format "Generated by `my-macro-region-or-at-point' with `%s'." (symbol-name target-func))))
    `(progn
       (defun ,(read fname)
           (txt)
           ,doc-string
           (interactive
            (list (cond
                   ((use-region-p) (buffer-substring (region-beginning) (region-end)))
                   (t (thing-at-point 'symbol)))))
           (if txt (,target-func txt)
             (,target-func))))))

(defmacro my-macro-region-or-at-point (target-func prompt)
  "Generate a function that invokes TARGET-FUNC by region when region specified,
fallback to manual input with default input from `thing-at-point' (prompt is PROMPT).
Generated function is TARGET-FUNC-region-or-point."
  (let ((fname (format "%s-region-or-at-point" (symbol-name target-func)))
        (doc-string (format "Generated by `my-macro-region-or-at-point' with `%s'." (symbol-name target-func))))
    `(progn
       (defun ,(read fname)
           (txt)
           ,doc-string
           (interactive
            (list (cond
                   ((use-region-p) (buffer-substring (region-beginning) (region-end)))
                   (t (read-string ,prompt (thing-at-point 'symbol))))))
           (,target-func txt)))))

(defmacro my-macro-buffer-or-region (target-func)
  "Generate a function that invokes TARGET-FUNC by region when region specified,
fallback to whole buffer.
Generated function is TARGET-FUNC-buffer-or-region."
  (let ((fname (format "%s-buffer-or-region" (symbol-name target-func)))
        (doc-string (format "Generated by `my-macro-buffer-or-region' with `%s'." (symbol-name target-func))))
    `(progn
       (defun ,(read fname)
           (txt)
         ,doc-string
         (interactive
          (list (cond
                 ((use-region-p) (buffer-substring (region-beginning) (region-end)))
                 (t (buffer-substring (point-min) (point-max))))))
         (,target-func txt)))))

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

(defmacro my-macro-handle-buffer (handler buffer-name)
  "Generate HANDLER-BUFFER-NAME ('*' deleted) function that apply HANDLER to BUFFER-NAME."
  (let ((fname (format "%s-%s"
                        (symbol-name handler)
                        (s-replace-all '(("*" . "")) buffer-name)))
         (doc-string (format "Generated by `my-macro-handle-buffer' with `%s' and %s"
                             (symbol-name handler) buffer-name)))
    `(progn
       (defun ,(read fname)
           ()
           ,doc-string
         (interactive)
         (,handler ,buffer-name)))))

(defmacro my-macro-ring-hook (name ring)
  "Generate a hook variable that calls hooked functions with a state in rotation.
The hook variable is NAME-ring-hook.
A hooked function takes 2 arguments, the current state and the next state,
the list of the states is NAME-ring-hook-states, initialized by RING.
Run the hook by calling NAME-ring-hook-rotate, that calls hooked functions
with the states and sets the next state to current.

For example,

  (my-macro-ring-hook \"test\" '(1 2 4))

generates a hook variable `test-ring-hook', an interactive function
`test-ring-hook-rotate' and a variable `test-ring-hook-states'
Add a function to the variable:

  (add-to-list 'test-ring-hook FUNCTION)

Run the hook by (test-ring-hook-rotate), then calls (HOOKED-FUNCTION 1 2).
Next (test-ring-hook-rotate) then calls (HOOKED-FUNCTION 2 4),
(HOOKED-FUNCTION 4 1), (HOOKED-FUNCTION 1 2), ..."
  (let ((hook-name (format "%s-ring-hook" name))
        (ring-name (format "%s-ring-hook-states" name))
        (state-index (format "%s-ring-hook-state-index" name))
        (rotate-func (format "%s-ring-hook-rotate" name))
        (get-func (format "%s-ring-hook-get-state" name))
        (incr-func (format "%s-ring-hook-incr-state-index" name))
        (doc-string (format "Generated by `my-macro-ring-hook' with %s" name)))
    `(progn
       (defvar ,(read hook-name)
         nil
         ,doc-string)
       (defvar ,(read ring-name)
         ,ring
         ,doc-string)
       (defvar ,(read state-index)
         0
         ,doc-string)
       (defun ,(read get-func)
           ()
         ,doc-string
         (nth ,(read state-index) ,(read ring-name)))
       (defun ,(read incr-func)
           ()
         ,doc-string
         (setq ,(read state-index)
               (mod (+ 1 ,(read state-index)) (length ,(read ring-name)))))
       (defun ,(read rotate-func)
           ()
         ,doc-string
         (interactive)
         (let* ((orig-state (,(read get-func)))
                (next-state (and (,(read incr-func))
                                 (,(read get-func)))))
           (cl-loop for f in ,(read hook-name)
                 do (funcall f orig-state next-state)))))))

(provide 'my-macro)
;;; my-macro.el ends here
