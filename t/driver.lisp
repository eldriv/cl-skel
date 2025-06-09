;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; driver.lisp --- top-level definitions for the tests

(uiop:define-package #:cl-project-builder/t/driver
  (:nicknames #:cl-proj-t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:cl-project-builder/t/run))

(provide "cl-project-builder/t")
(provide "CL-PROJECT-BUILDER/T")