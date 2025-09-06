;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; cl-skel-tests.asd --- ASDF test system for cl-skel

(defsystem #:cl-skel-tests
  :description "ASDF test system of CL-SKEL"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (#:fiveam
               #:marie
               #:cl-skel/t/utilities
               #:cl-skel/t/run
               #:cl-skel/t/driver)
  :perform (test-op (o c)
             (uiop:symbol-call :cl-skel/t/run :run-tests)))
