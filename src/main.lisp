(uiop:define-package #:cl-skel/src/main
  (:use #:cl
        #:marie
        #:cl-skel/src/templates/specials
        #:cl-skel/src/utilities
        #:cl-skel/src/templates/root
        #:cl-skel/src/templates/system-definitions
        #:cl-skel/src/templates/src-files
        #:cl-skel/src/templates/test-files))

(in-package #:cl-skel/src/main)

;;; Entry points / main entry


(def create-files (project project-dir)
  "Write the project files in PROJECT-DIR."
  (let ((project-source-dir (construct-path +src-directory+ project-dir))
        (project-tests-dir (construct-path +test-directory+ project-dir)))
    ;; Root files
    (with-out-files project-dir
      (("README" "org") (create-readme-file))
      (("makefile") (create-makefile))
      ((".gitignore") (create-gitignore-file))
      ((project "asd") (create-src-asdf))
      (((cat project #\- "tests") "asd") (create-t-asdf))
      (("version" "sexp") (create-src-version-file))
      (("version-tests" "sexp") (create-t-version-file)))
    ;; src files
    (with-out-files project-source-dir
      (("main" "lisp") (create-src-main-file))
      (("driver" "lisp") (create-src-driver))
      (("user" "lisp") (create-src-user))
      (("build" "lisp") (create-src-build))
      (("utilities" "lisp") (create-src-utilities))
      (("specials" "lisp") (create-src-specials)))
    ;; test files
    (with-out-files project-tests-dir
      (("main-tests" "lisp") (create-t-main-file))
      (("driver-tests" "lisp") (create-t-driver-file))
      (("user-tests" "lisp") (create-t-user-file)))))

(defm with-project-creation ((project target) &body body)
  "Execute BODY with project creation context and error handling."
  (let ((proj-sym (gensym "PROJECT"))
        (target-sym (gensym "TARGET")))
    `(let* ((,proj-sym (normalize-name ,project))
            (,target-sym ,target)
            (*cl-project* ,proj-sym))
       (unless (empty-string-p ,proj-sym)
         (handler-case
             (progn ,@body)
           (error () nil))))))

(def create-project^cr8 (project &key (target (home "common-lisp")))
  "Create a project using macro-based error handling."
  (handler-bind ((#+sbcl sb-int:simple-file-error
                  #+lispworks conditions:file-operation-error
                  #-(or sbcl lispworks) error
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'return-nil))))
    (with-project-creation (project target)
      (let ((project-dir (construct-path project target)))
        (create-directory-structure project-dir)
        (create-files project project-dir)
        (enable-project-registry project-dir)
        project-dir))))
