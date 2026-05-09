(uiop:define-package #:cl-skel/src/templates/src-files
  (:use #:cl #:cl-skel/src/utilities)
  (:export #:create-src-specials
           #:create-src-utilities
           #:create-src-main-file
           #:create-src-driver
           #:create-src-user
           #:create-src-build))

(in-package #:cl-skel/src/templates/src-files)

(deftemplate src-specials
    "Generate `/src/specials.lisp'."
  ";;;; specials.lisp --- Special variables

(uiop:define-package #:${project}/src/specials
  (:use #:cl))

(in-package #:${project}/src/specials)
")

(deftemplate src-utilities
    "Generate `/src/utilities.lisp'."
  ";;;; utilities.lisp --- Common utilities

(uiop:define-package #:${project}/src/utilities
  (:use #:cl))

(in-package #:${project}/src/utilities)
")

(deftemplate src-main-file
    "Generate `/src/main.lisp'."
  ";;;; main.lisp --- Entrypoints functions

(uiop:define-package #:${project}/src/main
  (:use #:cl)
  (:export #:hello))

(in-package #:${project}/src/main)

(defun hello ()
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
  (:use #:cl #:${project}/src/driver))

(in-package #:${project}-user)
")

(deftemplate src-build
    "Generate `/src/build.lisp'."
  "(require 'asdf)

(defun cwd-name ()
  (multiple-value-bind (type list &rest rest)
      (uiop:split-unix-namestring-directory-components
       (namestring (uiop:getcwd)))
    (declare (ignore type rest))
    (car (last list))))

(defun cwd-keyword ()
  (intern (cwd-name) (find-package :keyword)))

(push (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system (cwd-keyword))
(asdf:make (cwd-keyword))
")
