(uiop:define-package #:cl-project-builder/src/test-files
  (:use #:cl
        #:marie
        #:cl-project-builder/src/utilities))

(in-package #:cl-project-builder/src/test-files)

;;; Test Files Generation

(def create-t-version-file ()
  "Generate `/version-tests.sexp'."
  (fmt "\"0.0.1\""))

(def create-t-main-file ()
  "Generate `/t/main-tests.lisp'."
  (format-with-replacements  ";;;; main-tests.lisp -- main functions tests

(uiop:define-package #:${project}/t/main-tests
  (:use #:cl #:marie
        #:fiveam
        #:${project}))

(in-package #:${project}/t/main-tests)

(def run-tests ()
  \"Run all the tests defined in the suite.\"
  (run-all-tests))
"))

(def create-t-driver-file ()
  "Generate `/t/driver-tests.lisp'."
  (format-with-replacements  ";;;; driver-tests.lisp --- symbol driver tests

(uiop:define-package :${project}/t/driver-tests
  (:nicknames #:${project}/t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:${project}/t/main-tests))

(provide \"${project}/t\")
(provide \"${PROJECT}/T\")
"))

(def create-t-user-file ()
  "Generate `/t/user-tests.lisp'."
  (format-with-replacements  ";;;; user-tests.lisp --- user sandbox tests

(uiop:define-package :${project}/t/user-tests
  (:nicknames #:${project}-tests-user)
  (:use #:cl #:marie
        #:${project}/t/driver-tests))

(in-package #:${project}-tests-user)
"))
