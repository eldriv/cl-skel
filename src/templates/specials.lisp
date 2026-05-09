(uiop:define-package #:cl-skel/src/templates/specials
  (:use #:cl)
  (:export #:*table*
           #:+header+
           #:+src-directory+
           #:+test-directory+
           #:*cl-project*
           #:git-user-name
           #:git-user-email))

(in-package #:cl-skel/src/templates/specials)

(defun run-trim (command)
  "Run COMMAND and return trimmed output string."
  (let ((output (uiop:run-program command :output :string)))
    (string-trim '(#\newline #\tab #\space) output)))

(defun execute-with-restart (command)
  "Run COMMAND using RESTART-CASE to allow returning empty string on error."
  (restart-case (run-trim command)
    (return-empty-string ()
      "")))

(defun cmd-output (command)
  "Run COMMAND and return its output string; return \"\" on subprocess error."
  (handler-bind ((uiop/run-program:subprocess-error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'return-empty-string))))
    (execute-with-restart command)))

(defun preload-git-config (config-key)
  "Preload Git config value for CONFIG-KEY."
  (cmd-output (format nil "git config ~A || (cd && git config ~A)" config-key config-key)))

(defparameter *git-user-name*
  (preload-git-config "user.name")
  "Preloaded Git user name.")

(defparameter *git-user-email*
  (preload-git-config "user.email")
  "Preloaded Git user email.")

(defun git-user-name (&rest args)
  "Return the Git user name."
  (declare (ignore args))
  *git-user-name*)

(defun git-user-email (&rest args)
  "Return the Git user email."
  (declare (ignore args))
  *git-user-email*)

(defvar *table*
  '(("${project}" . nil)
    ("${PROJECT}" . string-upcase)
    ("${author}"  . git-user-name)
    ("${email}"   . git-user-email)))

;; Use DEFPARAMETER (not DEFCONSTANT): SBCL errors on DEFCONSTANT-UNEQL
;; when the string/pathname value changes across incremental recompiles.
(defparameter +header+
  ";;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-"
  "Lint mode line prefixed to emitted templates.")

(defparameter +src-directory+ #P"src"
  "The main source directory.")

(defparameter +test-directory+ #P"t"
  "The main tests directory.")

(defvar *cl-project* "cl-project"
  "The default project name.")
