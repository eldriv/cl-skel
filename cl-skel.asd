;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; cl-skel.asd --- top-level ASDF file for cl-skel


(defsystem #:cl-skel
  :version (:read-file-form #P"version.sexp")
  :description "A Common Lisp project generator (portable ASDF/UIOP only)."
  :author "Eldriv"
  :maintainer "Eldriv"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (#:uiop
               #:cl-skel/src/templates/specials
               #:cl-skel/src/utilities
               #:cl-skel/src/templates/root
               #:cl-skel/src/templates/system-definitions
               #:cl-skel/src/templates/src-files
               #:cl-skel/src/templates/test-files
               #:cl-skel/src/main)
  :in-order-to ((test-op (test-op "cl-skel-tests"))))
