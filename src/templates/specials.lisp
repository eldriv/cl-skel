(uiop:define-package #:cl-skel/src/templates/specials
  (:use #:cl
        #:marie))

(in-package #:cl-skel/src/templates/specials)

;;; Uitilities
(def run-trim (command)
  "Run COMMAND and return trimmed output string."
  (let ((output (uiop:run-program command :output :string)))
    (string-trim '(#\newline #\tab #\space) output)))


(def execute-with-restart (command)
  "Run COMMAND using RESTART-CASE to allow returning empty string on error."
  (restart-case (run-trim command)
    (return-empty-string ()
      "")))

(def cmd-output (command)
  "Run COMMAND and return its output string; return \"\" on subprocess error."
  (handler-bind ((uiop/run-program:subprocess-error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'return-empty-string))))
    (execute-with-restart command)))


(def preload-git-config (config-key)
  "Preload Git config value for CONFIG-KEY."
  (cmd-output (format nil "git config ~A || (cd && git config ~A)" config-key config-key)))

(defn sub-process-error (error)
    ((text :initarg :text :reader text))
    (:documentation "Condition for subprocess errors."))

(defp *git-user-name*
    (preload-git-config "user.name")
  "Preloaded Git user name.")

(defp *git-user-email*
    (preload-git-config "user.email")
  "Preloaded Git user email.")

(def git-user-name (&rest args)
  "Return the Git user name."
  (declare (ignore args))
  *git-user-name*)

(def git-user-email (&rest args)
  "Return the Git user email."
  (declare (ignore args))
  *git-user-email*)

(defv *table*
    '(("${project}" . nil)
      ("${PROJECT}" . string-upcase)
      ("${author}"  . git-user-name)
      ("${email}"   . git-user-email)))

;;; Configuration and Constants

(defk +header+
  ";;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-")

(defk +src-directory+
  #P"src"
  "The main source directory.")

(defk +test-directory+
  #P"t"
  "The main tests directory.")

(defv *cl-project* "cl-project"
  "The default project name.")
