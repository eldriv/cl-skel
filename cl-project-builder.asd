;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; cl-project-builder.asd --- top-level ASDF file for cl-project-builder
(defsystem #:cl-project-builder
  :version (:read-file-form #P"version.lisp")
  :description "A Modern Common Lisp Project Generator with Marie's Support utilities."
  :author "Eldriv"
  :maintainer "Eldriv"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (#:uiop
               #:marie
               #:cl-project-builder/src/specials
               #:cl-project-builder/src/utilities
               #:cl-project-builder/src/root
               #:cl-project-builder/src/system-definitions
               #:cl-project-builder/src/src-files
               #:cl-project-builder/src/test-files
               #:cl-project-builder/src/main)
  :in-order-to ((test-op (test-op "proj-test"))))
