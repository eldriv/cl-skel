;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; run.lisp --- test entrypoint for ASDF TEST-OP

(uiop:define-package #:cl-skel/t/run
  (:use #:cl #:cl-skel/t/utilities)
  (:export #:run-tests))

(in-package #:cl-skel/t/run)

(defun run-tests ()
  "Run all tests; signal an error if any assertion fails."
  (dolist (thunk (list #'test-normalize-name #'test-format-with-replacements
                       #'test-construct-path-tree))
    (funcall thunk))
  (format t "~&All cl-skel tests passed.~%"))
