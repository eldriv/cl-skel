(uiop:define-package #:cl-skel/src/main
  (:use #:cl
        #:cl-skel/src/templates/specials
        #:cl-skel/src/utilities
        #:cl-skel/src/templates/root
        #:cl-skel/src/templates/system-definitions
        #:cl-skel/src/templates/src-files
        #:cl-skel/src/templates/test-files)
  (:export #:cr8))

(in-package #:cl-skel/src/main)

(defun empty-string-p (s)
  (or (null s) (and (stringp s) (zerop (length s)))))

(defun home-subpath (relative-dir)
  "Directory under the user home, e.g. \"common-lisp/\"."
  (merge-pathnames relative-dir (user-homedir-pathname)))

(defun create-files (project project-dir)
  "Write the project files in PROJECT-DIR."
  (reset-cr8-progress 15)
  (let ((project-source-dir (construct-path +src-directory+ project-dir))
        (project-tests-dir (construct-path +test-directory+ project-dir)))
    (with-out-files project-dir
      (("README" "org") (create-readme-file))
      (("makefile") (create-makefile))
      ((project "asd") (create-src-asdf))
      (((concatenate 'string project "-" "tests") "asd") (create-t-asdf))
      (("version" "sexp") (create-src-version-file))
      (("version-tests" "sexp") (create-t-version-file)))
    (with-out-files project-source-dir
      (("main" "lisp") (create-src-main-file))
      (("driver" "lisp") (create-src-driver))
      (("user" "lisp") (create-src-user))
      (("build" "lisp") (create-src-build))
      (("utilities" "lisp") (create-src-utilities))
      (("specials" "lisp") (create-src-specials)))
    (with-out-files project-tests-dir
      (("main-tests" "lisp") (create-t-main-file))
      (("driver-tests" "lisp") (create-t-driver-file))
      (("user-tests" "lisp") (create-t-user-file)))))

(defmacro with-project-creation ((project target) &body body)
  "Execute BODY with *CL-PROJECT* set from normalized PROJECT name."
  (declare (ignorable project target))
  (let ((proj-sym (gensym "PROJECT")))
    `(let* ((,proj-sym (normalize-name ,project))
            (*cl-project* ,proj-sym))
       (unless (empty-string-p ,proj-sym)
         ,@body))))

(defun create-project (project &key (target (home-subpath "common-lisp/")))
  "Create a project; return PROJECT-DIR or NIL on failure."
  (handler-case
      (with-project-creation (project target)
        (let ((project-dir (construct-path project target)))
          (create-directory-structure project-dir)
          (create-files project project-dir)
          (enable-project-registry project-dir)
          project-dir))
    ;; Each #+ must cover one full clause form, not only the typespec.
    #+sbcl (sb-int:simple-file-error (c) (declare (ignore c)) nil)
    #+lispworks (conditions:file-operation-error (c) (declare (ignore c)) nil)))

(defun cr8 (project &key (target (home-subpath "common-lisp/")))
  "Create a project (public alias for CREATE-PROJECT)."
  (create-project project :target target))
