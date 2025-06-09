(uiop:define-package #:cl-project-builder/src/root
  (:use #:cl
        #:marie
        #:cl-project-builder/src/utilities))

(in-package #:cl-project-builder/src/root)

(def create-src-version-file ()
  "Generate `/version.sexp'."
  (fmt "\"1.0.0\""))

(def create-t-version-file ()
  "Generate `/version.sexp'."
  (fmt "\"1.0.0\""))

(deftemplate* readme-file
    "Generate `/Readme.md'."
  "#+title: ${project}
#+author: ${author}
#+email: ${email}
")

(deftemplate* gitignore-file
    "Generate `/.gitignore'."
  "*.fasl
*.64yfasl
*.lisp-temp
*.dfsl
*.pfsl
*.d64fsl
*.p64fsl
*.lx64fsl
*.lx32fsl
*.dx64fsl
*.dx32fsl
*.fx64fsl
*.fx32fsl
*.sx64fsl
*.sx32fsl
*.wx64fsl
*.wx32fsl
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
	@$(LISP) --eval '(ql:quickload :${project})' --eval '(asdf:make :${project})' --eval '(uiop:quit)'

clean:
	@rm -f $(NAME)
")
