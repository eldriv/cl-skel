(uiop:define-package #:cl-skel/src/templates/root
  (:use #:cl #:cl-skel/src/utilities)
  (:export #:create-readme-file
           #:create-makefile
           #:create-src-version-file
           #:create-t-version-file))

(in-package #:cl-skel/src/templates/root)

(defun create-src-version-file ()
  "Generate `/version.sexp'."
  (format nil "\"1.0.0\""))

(defun create-t-version-file ()
  "Generate `/version-tests.sexp'."
  (format nil "\"1.0.0\""))

(deftemplate* readme-file
    "Generate `/README.org'."
  "#+title: ${project}
#+author: ${author}
#+email: ${email}
")

(deftemplate* makefile
    "Generate `/Makefile'."
  "SHELL := bash
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:

NAME = ${project}
LISP := sbcl

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	@$(LISP) --non-interactive \\
		--eval '(require \"asdf\")' \\
		--eval '(push (uiop:ensure-directory-pathname (uiop:getcwd)) asdf:*central-registry*)' \\
		--eval '(asdf:load-system :${project})' \\
		--eval '(asdf:make :${project})' \\
		--eval '(uiop:quit 0)'

clean:
	@rm -f $(NAME)
")
