(uiop:define-package #:cl-skel/src/utilities
  (:use #:cl
        #:marie
        #:cl-skel/src/templates/specials))

(in-package #:cl-skel/src/utilities)

;;; Strings and pathnames helpers

(def construct-path (path1 path2)
  "Return a new path ensuring that PATH2 is a directory."
  (merge-pathnames path1 (uiop:ensure-directory-pathname path2)))

(def substitute-all (string part replacement &key (test #'char=))
  "Return a new string in which all occurrences of PART in STRING are replaced with REPLACEMENT."
  (with-output-to-string (out)
    (let ((part-length (length part))
          (string-length (length string)))
      (do ((start 0))
          ((>= start string-length))
        (let ((pos (search part string :start2 start :test test)))
          (if pos
              (progn
                ;; Write part before match
                (write-string string out :start start :end pos)
                ;; Write replacement
                (write-string replacement out)
                (setf start (+ pos part-length)))
              (progn
                ;; Write remaining string
                (write-string string out :start start :end string-length)
                (setf start string-length))))))))

(def lookup-replacement-fn (string)
  "Return the transformation function for STRING."
  (assoc-value string *table*))

(def apply-replacement (marker string)
  (let* ((fn (lookup-replacement-fn marker))
         (str (if fn (funcall fn string) string)))
    str))

(def perform-replacement (string marker subst)
  "Replace STRING with SUBST."
  (let ((replacement (apply-replacement marker subst)))
    (substitute-all string marker replacement)))

(def perform-all-replacements (string subst)
  "Perform string replacements blah blah blah."
  (let ((markers (mapcar #'car *table*)))
    (reduce (lambda (str marker)
              (perform-replacement str marker subst))
            markers
            :initial-value string)))

(def format-with-replacements (string &key project no-header)
  "Return a string with pre-defined substitutions."
  (perform-all-replacements
   (if no-header (fmt "~A" string) (fmt "~A~%~A" +header+ string))
   (or project *cl-project*)))

(def format-with-replacements* (&rest args)
  "Like, FORMAT-WITH-REPLACEMENTS, but without HEADERS."
  (apply #'format-with-replacements (append args (list :no-header t))))


;;; File System Helpers

(defm with-out-files (dir &body file-specs)
  "Generate multiple files inside DIR using *cr8-file*."
  `(uiop:with-current-directory (,dir)
     ,@(loop for (fname contents) in file-specs
             collect `(funcall *cr8-file* ,(if (listp fname)
                                               `(path ,@fname)
                                               `(path ,fname))
                       ,contents))))

(defp *cr8-file* (cr8-file)
  "Create an instance of the file generator")

(def cr8-file ()
  "Return a cr8-file function with an internal counter."
  (let ((counter 0))
    (lambda (path contents)
      (incf counter)
      (let* ((cwd (uiop:getcwd))
             (abspath (merge-pathnames path cwd))
             (logs (format nil "[~a/16] — ~a~%" counter (namestring abspath))))
        (format t "~a" logs)  ;; print the log
        (with-open-file (out abspath
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "~a" contents))))))

(def path (name &optional type)
  "Return a pathname from NAME and TYPE."
  (make-pathname :name name :type type))

(def enable-project-registry (project-dir)
  "Make the system indicated by PROJECT-DIR immediately accessible by ASDF."
  (push (uiop:ensure-directory-pathname project-dir) asdf:*central-registry*)
  (uiop:ensure-directory-pathname project-dir))

(def normalize-name (name)
  "Return a new string from NAME suitable as a project name."
  (string-downcase (string name)))

(def create-directory-structure (target)
  "Create the directory structure of the new project under TARGET."
  (let ((target-dir (uiop:ensure-directory-pathname target))
        (directories (list +src-directory+ +test-directory+)))
    (loop :for dir :in directories
          :for path := (construct-path (uiop:ensure-directory-pathname dir) target-dir)
          :do (handler-case
                  (uiop:ensure-all-directories-exist (list path))
                (error (e)
                  (warn "Failed to create directory ~A: ~A" path e))))))

;;; Template Macros

(defm deftemplate (name description &body template-body)
  "Define a template generation function with NAME, DESCRIPTION, and TEMPLATE-BODY.
   The function will be named CREATE-<NAME> and will generate the specified template."
  (let ((func-name (intern (format nil "CREATE-~A" (symbol-name name)) *package*)))
    `(def ,func-name ()
       ,description
       (format-with-replacements ,(first template-body)))))


(defm deftemplate* (name description &body template-body)
  "Like DEFTEMPLATE but uses FORMAT-WITH-REPLACEMENTS* (no header)."
  (let ((func-name (intern (format nil "CREATE-~A" (symbol-name name)) *package*)))
    `(def ,func-name ()
       ,description
       (format-with-replacements* ,(first template-body)))))
