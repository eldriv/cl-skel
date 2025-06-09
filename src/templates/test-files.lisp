(uiop:define-package #:cl-project-builder/src/templates/test-files
  (:use #:cl
        #:marie
        #:cl-project-builder/src/utilities))

(in-package #:cl-project-builder/src/templates/test-files)

;;; Test Files Generation

(def t-version-file ()
  "Generate `/version-tests.sexp'."
  (fmt "\"1.0.0\""))

(deftemplate t-main-file
    "Generate `/main-tests.lisp'."
  ";;;; main-tests.lisp -- Main functions tests

(uiop:define-package #:${project}/t/main-tests
  (:use #:cl #:marie
        #:fiveam
        #:${project}))

(in-package #:${project}/t/main-tests)

(def run-tests ()
  \"Run all the tests defined in the suite.\"
  (run-all-tests))
")

(deftemplate t-driver-file
    "Generate `/driver-tests.lisp'."
  ";;;; driver-tests.lisp --- Driver tests

(uiop:define-package :${project}/t/driver-tests
  (:nicknames #:${project}/t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:${project}/t/main-tests))

(provide \"${project}/t\")
(provide \"${PROJECT}/T\")
")

(deftemplate t-user-file
    "Generate `/users-tests.lisp'."
  ";;;; user-tests.lisp --- User playground tests

(uiop:define-package :${project}/t/user-tests
  (:nicknames #:${project}-tests-user)
  (:use #:cl #:marie
        #:${project}/t/driver-tests))

(in-package #:${project}-tests-user)")
