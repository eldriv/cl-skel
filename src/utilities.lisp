(uiop:define-package #:cl-skel/src/utilities
  (:use #:cl #:cl-skel/src/templates/specials)
  (:export #:construct-path
           #:format-with-replacements
           #:format-with-replacements*
           #:*cr8-file*
           #:with-out-files
           #:path
           #:enable-project-registry
           #:normalize-name
           #:create-directory-structure
           #:deftemplate
           #:deftemplate*
           #:reset-cr8-progress))

(in-package #:cl-skel/src/utilities)

(defun assoc-value (key alist &key (test #'equal))
  (cdr (assoc key alist :test test)))

(defun fmt (control-string &rest args)
  (apply #'format nil control-string args))

;;; Strings and pathnames helpers

(defun construct-path (path1 path2)
  "Return PATH1 as a directory under PATH2 (PATH2 must be a directory).
PATH1 is a string (one path segment, e.g. project name) or a pathname
such as #P\"src\" meaning a subdirectory named src — not a file named src."
  (let ((base (uiop:ensure-directory-pathname path2)))
    (etypecase path1
      (string
       (make-pathname :defaults base
                      :directory (append (pathname-directory base) (list path1))))
      (pathname
       (cond
         ;; #P\"name\" with no directory: treat NAME as one relative directory component.
         ((and (pathname-name path1)
               (null (pathname-directory path1)))
          (make-pathname :defaults base
                         :directory (append (pathname-directory base)
                                            (list (pathname-name path1)))))
         (t
          (merge-pathnames (uiop:ensure-directory-pathname path1) base)))))))

(defun substitute-all (string part replacement &key (test #'char=))
  "Return a new string in which all occurrences of PART in STRING are replaced with REPLACEMENT."
  (with-output-to-string (out)
    (let ((part-length (length part))
          (string-length (length string)))
      (do ((start 0))
          ((>= start string-length))
        (let ((pos (search part string :start2 start :test test)))
          (if pos
              (progn
                (write-string string out :start start :end pos)
                (write-string replacement out)
                (setf start (+ pos part-length)))
              (progn
                (write-string string out :start start :end string-length)
                (setf start string-length))))))))

(defun lookup-replacement-fn (string)
  "Return the transformation function for STRING."
  (assoc-value string *table* :test #'string=))

(defun apply-replacement (marker string)
  (let* ((fn (lookup-replacement-fn marker))
         (str (if fn (funcall fn string) string)))
    str))

(defun perform-replacement (string marker subst)
  "Replace STRING with SUBST."
  (let ((replacement (apply-replacement marker subst)))
    (substitute-all string marker replacement)))

(defun perform-all-replacements (string subst)
  (let ((markers (mapcar #'car *table*)))
    (reduce (lambda (str marker)
              (perform-replacement str marker subst))
            markers
            :initial-value string)))

(defun format-with-replacements (string &key project no-header)
  "Return a string with pre-defined substitutions."
  (perform-all-replacements
   (if no-header (fmt "~A" string) (fmt "~A~%~A" +header+ string))
   (or project *cl-project*)))

(defun format-with-replacements* (&rest args)
  "Like FORMAT-WITH-REPLACEMENTS, but without HEADERS."
  (apply #'format-with-replacements (append args (list :no-header t))))

;;; File creation progress (bound in CREATE-FILES)

(defvar *cr8-file-counter* 0)
(defvar *cr8-file-total* 0)

(defun reset-cr8-progress (total)
  (setf *cr8-file-counter* 0
        *cr8-file-total* total))

(defun cr8-file-fn (path contents)
  (incf *cr8-file-counter*)
  (let* ((cwd (uiop:getcwd))
         (abspath (merge-pathnames path cwd))
         (logs (format nil "[~a/~a] — ~a~%"
                       *cr8-file-counter* *cr8-file-total*
                       (namestring abspath))))
    (format t "~a" logs)
    (with-open-file (out abspath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "~a" contents))))

(defparameter *cr8-file* #'cr8-file-fn
  "Write one generated file (see WITH-OUT-FILES).")

(defmacro with-out-files (dir &body file-specs)
  "Generate multiple files inside DIR using *CR8-FILE*."
  `(uiop:with-current-directory (,dir)
     ,@(loop for (fname contents) in file-specs
             collect `(funcall *cr8-file* ,(if (listp fname)
                                               `(path ,@fname)
                                               `(path ,fname))
                       ,contents))))

(defun path (name &optional type)
  "Return a pathname from NAME and TYPE."
  (make-pathname :name name :type type))

(defun enable-project-registry (project-dir)
  "Make the system indicated by PROJECT-DIR immediately accessible by ASDF."
  (push (uiop:ensure-directory-pathname project-dir) asdf:*central-registry*)
  (uiop:ensure-directory-pathname project-dir))

(defun normalize-name (name)
  "Return a new string from NAME suitable as a project name."
  (string-downcase (string name)))

(defun create-directory-structure (target)
  "Create the directory structure of the new project under TARGET."
  (let ((target-dir (uiop:ensure-directory-pathname target))
        (directories (list +src-directory+ +test-directory+)))
    (loop :for dir :in directories
          :for path := (construct-path (uiop:ensure-directory-pathname dir) target-dir)
          :do (handler-case
                  (uiop:ensure-all-directories-exist (list path))
                (error (e)
                  (warn "Failed to create directory ~A: ~A" path e))))))

;;; Template macros

(defmacro deftemplate (name description &body template-body)
  "Define a template generation function with NAME, DESCRIPTION, and TEMPLATE-BODY."
  (let ((func-name (intern (format nil "CREATE-~A" (symbol-name name)) *package*)))
    `(defun ,func-name ()
       ,description
       (format-with-replacements ,(first template-body)))))

(defmacro deftemplate* (name description &body template-body)
  "Like DEFTEMPLATE but uses FORMAT-WITH-REPLACEMENTS* (no header)."
  (let ((func-name (intern (format nil "CREATE-~A" (symbol-name name)) *package*)))
    `(defun ,func-name ()
       ,description
       (format-with-replacements* ,(first template-body)))))
