(uiop:define-package #:cl-project-builder/src/src-files
  (:use #:cl
        #:marie
        #:cl-project-builder/src/utilities))

(in-package #:cl-project-builder/src/src-files)


;; Source Files Generation

(def create-src-specials ()
  "Generate `/src/specials.lisp'."
  (format-with-replacements ";;;; specials.lisp --- special variables

(uiop:define-package #:${project}/src/specials
  (:use #:cl
        #:marie))

(in-package #:${project}/src/specials)
"))

(def create-src-utilities ()
  "Generate `/src/utilities.lisp'."
  (format-with-replacements ";;;; utilities.lisp --- common utilities

(uiop:define-package #:${project}/src/utilities
  (:use #:cl
        #:marie))

(in-package #:${project}/src/utilities)
"))

(def create-src-main-file ()
  "Generate `/src/main.lisp'."
  (format-with-replacements ";;;; main.lisp --- entrypoints functions

(uiop:define-package #:${project}/src/main
  (:use #:cl
        #:marie))

(in-package #:${project}/src/main)

(def main^hello ()
  \"Display a greeting.\"
  (format t \"Hello, world!~%\"))
"))

(def create-src-driver ()
  "Generate `/src/driver.lisp'."
  (format-with-replacements ";;;; driver.lisp --- symbol driver

(uiop:define-package #:${project}/src/driver
  (:nicknames #:${project})
  (:use #:uiop/common-lisp)
  (:use-reexport #:${project}/src/main))

(provide \"${project}\")
(provide \"${PROJECT}\")
"))

(def create-src-user ()
  "Generate `/src/user.lisp'."
  (format-with-replacements ";;;; user.lisp --- user sandbox

(uiop:define-package #:${project}/src/user
  (:nicknames #:${project}-user)
  (:use #:cl
        #:marie
        #:${project}/src/driver))

(in-package #:${project}-user)
"))

(def create-src-build ()
  "Generate `/src/build.lisp'."
  (format-with-replacements "(require 'asdf)
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
"))
