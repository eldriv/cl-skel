(uiop:define-package #:cl-skel/src/templates/test-files
  (:use #:cl #:cl-skel/src/utilities)
  (:export #:create-t-main-file
           #:create-t-driver-file
           #:create-t-user-file))

(in-package #:cl-skel/src/templates/test-files)

(deftemplate t-main-file
    "Generate `/main-tests.lisp'."
  ";;;; main-tests.lisp -- Main functions tests

(uiop:define-package #:${project}/t/main-tests
  (:use #:cl #:${project}))

(in-package #:${project}/t/main-tests)

(defun run-tests ()
  \"Run all tests; used by ASDF TEST-OP.\"
  (dolist (thunk (list #'test-hello))
    (funcall thunk))
  (format t \"~&All tests passed.~%\"))

(defun test-hello ()
  (assert (string= (format nil \"Hello, world!~%\")
                   (with-output-to-string (*standard-output*)
                     (hello)))))
")

(deftemplate t-driver-file
    "Generate `/driver-tests.lisp'."
  ";;;; driver-tests.lisp --- Driver tests

(uiop:define-package #:${project}/t/driver-tests
  (:nicknames #:${project}/t)
  (:use #:uiop/common-lisp)
  (:use-reexport #:${project}/t/main-tests))

(provide \"${project}/t\")
(provide \"${PROJECT}/T\")
")

(deftemplate t-user-file
    "Generate `/users-tests.lisp'."
  ";;;; user-tests.lisp --- User playground tests

(uiop:define-package #:${project}/t/user-tests
  (:nicknames #:${project}-tests-user)
  (:use #:cl #:${project}/t/driver-tests))

(in-package #:${project}-tests-user)")
