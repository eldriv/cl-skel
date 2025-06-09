(uiop:define-package #:cl-project-builder/src/src-files
  (:use #:cl
        #:marie
        #:cl-project-builder/src/utilities))

(in-package #:cl-project-builder/src/src-files)


;; Source Files Generation

(deftemplate src-specials
    "Generate `/src/specials.lisp'."
  ";;;; specials.lisp --- Special variables

(uiop:define-package #:${project}/src/specials
  (:use #:cl
        #:marie))

(in-package #:${project}/src/specials)
")

(deftemplate src-utilities
    "Generate `/src/utilities.lisp'."
  ";;;; utilities.lisp --- Common utilities

(uiop:define-package #:${project}/src/utilities
  (:use #:cl
        #:marie))

(in-package #:${project}/src/utilities)
")

(deftemplate src-main-file
    "Generate `/src/main.lisp'."
  ";;;; main.lisp --- Entrypoints functions

(uiop:define-package #:${project}/src/main
  (:use #:cl
        #:marie))

(in-package #:${project}/src/main)

(def main^hello ()
  \"Display a greeting.\"
  (format t \"Hello, world!~%\"))
")

(deftemplate src-driver
    "Generate `/src/driver.lisp'."
  ";;;; driver.lisp --- Symbol driver

(uiop:define-package #:${project}/src/driver
  (:nicknames #:${project})
  (:use #:uiop/common-lisp)
  (:use-reexport #:${project}/src/main))

(provide \"${project}\")
(provide \"${PROJECT}\")
")

(deftemplate src-user
    "Generate `/src/user.lisp'."
  ";;;; user.lisp --- User playground

(uiop:define-package #:${project}/src/user
  (:nicknames #:${project}-user)
  (:use #:cl
        #:marie
        #:${project}/src/driver))

(in-package #:${project}-user)
")

(deftemplate src-build
    "Generate `/src/build.lisp'."
  "(require 'asdf)
(defun cwd-name ()
  (multiple-value-bind (type list &rest rest)
      (uiop:split-unix-namestring-directory-components
       (namestring (uiop:getcwd)))
    (car (last list))))
(defun cwd-keyword () (intern (cwd-name) (find-package :keyword)))
(defun home (path) (merge-pathnames path (user-homedir-pathname)))
#-quicklisp (load (home \"quicklisp/setup.lisp\"))
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload (cwd-keyword))
(asdf:make (cwd-keyword))
")
