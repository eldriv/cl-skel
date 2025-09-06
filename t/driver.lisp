;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; driver.lisp --- top-level definitions for the tests

(uiop:define-package #:cl-skel/t/driver
  (:nicknames #:cl-skel-t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:cl-skel/t/run))

(provide "cl-skel/t")
(provide "CL-SKEL/T")
