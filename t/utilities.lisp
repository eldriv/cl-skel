;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; utilities.lisp --- unit test thunks for cl-skel

(uiop:define-package #:cl-skel/t/utilities
  (:use #:cl
        #:cl-skel/src/utilities
        #:cl-skel/src/templates/specials)
  (:export #:test-normalize-name
           #:test-format-with-replacements
           #:test-construct-path-tree))

(in-package #:cl-skel/t/utilities)

(defun test-normalize-name ()
  (assert (string= "foo" (normalize-name "FOO")))
  (assert (string= "bar" (normalize-name 'bar))))

(defun test-format-with-replacements ()
  (let ((*cl-project* "myapp"))
    (assert (search "myapp"
                    (format-with-replacements "${project}" :no-header t)))))

(defun test-construct-path-tree ()
  "Generated projects must be project/<root files> + src/ + t/ (regression for merge-pathnames layout)."
  (let* ((root (uiop:ensure-directory-pathname #p"/tmp/"))
         (proj (construct-path "cl-skel-layout-x" root))
         (src (construct-path +src-directory+ proj))
         (tdir (construct-path +test-directory+ proj))
         (ns (lambda (p) (namestring (uiop:ensure-directory-pathname p)))))
    (assert (search "cl-skel-layout-x" (funcall ns proj)))
    (assert (search "cl-skel-layout-x/src" (funcall ns src)))
    (assert (search "cl-skel-layout-x/t" (funcall ns tdir)))))
