(uiop:define-package #:cl-skel/src/templates/system-definitions
  (:use #:cl
        #:marie
        #:cl-skel/src/utilities))

(in-package #:cl-skel/src/templates/system-definitions)

(deftemplate* src-asdf
    "Generate `/my-project.asd'."
  ";;;; ${project}.asd --- Top-level ASDF file for ${project}
(defsystem #:${project}
    :description \"${project}\"
    :version (:read-file-form #P\"version.sexp\")
    :author \"${author} <${email}>\"
    :maintainer \"${author} <${email}>\"
    :license \"\"
    :class :package-inferred-system
    :depends-on (#:marie
                 #:${project}/src/main
                 #:${project}/src/driver
                 #:${project}/src/user)
    :in-order-to ((test-op (test-op \"${project}-tests\"))))
")

(deftemplate* t-asdf
    "Generate `/t-my-project.asd'."
  ";;;; ${project}-tests.asd --- Test ASDF file for ${project}
(defsystem #:${project}-tests
    :description \"\"
    :version (:read-file-form #P\"version-tests.sexp\")
    :author \"${author} <${email}>\"
    :maintainer \"${author} <${email}>\"
    :license \"\"
    :class :package-inferred-system
    :depends-on (#:fiveam
                 #:marie
                 #:${project}
                 #:${project}/t/main-tests
                 #:${project}/t/driver-tests
                 #:${project}/t/user-tests)
    :perform (test-op (o c) (uiop:symbol-call :${project}/t/main-tests :run-tests)))
")
