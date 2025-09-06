;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; run.lisp --- main file for the unit tests

(uiop:define-package #:cl-skel/t/run
  (:use #:cl
        #:cl-skel/t/utilities
        #:fiveam
        #:marie))

(in-package #:cl-skel/t/run)

;;; entrypoint

(def run-tests ()
  "Run all the tests defined in the suite."
  (run-all-tests))
