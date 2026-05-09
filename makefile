# === HEAD ===
SHELL := $(shell which bash 2>/dev/null || which sh)
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.ONESHELL:
.SHELLFLAGS := -c
.DELETE_ON_ERROR:

# SBCL: override with `make LISP=/path/to/sbcl`. Otherwise PATH, then common install locations.
LISP ?= $(shell sh -c 'c="$$(command -v sbcl 2>/dev/null)"; \
	if [ -n "$$c" ] && [ -x "$$c" ]; then echo "$$c"; \
	elif [ -x /usr/bin/sbcl ]; then echo /usr/bin/sbcl; \
	elif [ -x /usr/local/bin/sbcl ]; then echo /usr/local/bin/sbcl; \
	elif [ -x /opt/homebrew/bin/sbcl ]; then echo /opt/homebrew/bin/sbcl; \
	else echo sbcl; fi')

PROJECT_BUILDER := cl-skel
PROJECT_PACKAGE := cl-skel/src/main
TARGET_DIR := $(HOME)/common-lisp

# === Internal macro: load cl-skel via ASDF (no Quicklisp) ===
define generate_project
	echo "⚡ Generating project structure..."
	$(LISP) --non-interactive \
		--eval '(require "asdf")' \
		--eval '(push (uiop:ensure-directory-pathname (uiop:getcwd)) asdf:*central-registry*)' \
		--eval '(asdf:load-system :$(PROJECT_BUILDER))' \
		--eval '(in-package :$(PROJECT_PACKAGE))' \
		--eval '(cr8 "$(1)")' \
		--eval '(format t "✅ Project '\''$(1)'\'' created successfully!~%")' \
		--eval '(format t "📂 Location: $(TARGET_DIR)/$(1)~%")' \
		--eval '(uiop:quit 0)' || { \
			echo "❌ Error: Failed to create project. Run from $(PROJECT_BUILDER) repo root with SBCL + ASDF."; \
			exit 1; \
		}
endef

# === BODY ===
.PHONY: all project project-name setup assert-lisp

assert-lisp:
	@case "$(LISP)" in \
	  */*) test -x "$(LISP)" || { echo "❌ SBCL not executable: $(LISP)"; exit 1; } ;; \
	  *) command -v "$(LISP)" >/dev/null 2>&1 || { \
		echo "❌ SBCL not found (tried: $(LISP))."; \
		echo "   Install SBCL for your OS (e.g. apt install sbcl, dnf install sbcl, pacman -S sbcl, brew install sbcl)"; \
		echo "   or download from https://www.sbcl.org/platform-table.html — then: make setup LISP=/path/to/sbcl"; \
		exit 1; }; \
	esac

all: project

project: assert-lisp
	@echo "[Common Lisp Project Generator]"
	@echo ""
	@printf "📝 What is the name of the project?: "
	@read PROJECT_NAME && \
	if [ -z "$$PROJECT_NAME" ]; then \
		echo "❌ Error: Project name cannot be empty."; \
		exit 1; \
	fi && \
	echo "" && \
	echo "📂 Creating project: $$PROJECT_NAME" && \
	echo "📁 Target directory: $(TARGET_DIR)/$$PROJECT_NAME" && \
	echo "" && \
	if [ -d "$(TARGET_DIR)/$$PROJECT_NAME" ]; then \
		echo "⚠️  Warning: Directory $(TARGET_DIR)/$$PROJECT_NAME already exists."; \
		printf "Continue? (y/n): "; \
		read CONFIRM; \
		if [ "$$CONFIRM" != "y" ] && [ "$$CONFIRM" != "Y" ]; then \
			echo "🛑 Aborted."; \
			exit 1; \
		fi; \
	fi && \
	$(call generate_project,$$PROJECT_NAME)

project-name: assert-lisp
	@if [ -z "$(PROJECT)" ]; then \
		echo "❌ Error: PROJECT variable not set. Use: make project-name PROJECT=my-project"; \
		exit 1; \
	fi
	@echo "📂 Creating project: $(PROJECT)"
	@echo "📁 Target directory: $(TARGET_DIR)/$(PROJECT)"
	@if [ -d "$(TARGET_DIR)/$(PROJECT)" ]; then \
		echo "⚠️  Warning: Directory $(TARGET_DIR)/$(PROJECT) already exists."; \
		printf "Continue? (y/n): "; \
		read CONFIRM; \
		if [ "$$CONFIRM" != "y" ] && [ "$$CONFIRM" != "Y" ]; then \
			echo "🛑 Aborted."; \
			exit 1; \
		fi; \
	fi
	$(call generate_project,$(PROJECT))

setup: assert-lisp
	@echo "⚙️  Setting up $(PROJECT_BUILDER)..."
	@if [ ! -f "$(PROJECT_BUILDER).asd" ]; then \
		echo "❌ Error: $(PROJECT_BUILDER).asd not found in current directory."; \
		echo "Please run this makefile from the $(PROJECT_BUILDER) root directory."; \
		exit 1; \
	fi
	@$(LISP) --non-interactive \
		--eval '(require "asdf")' \
		--eval '(push (uiop:ensure-directory-pathname (uiop:getcwd)) asdf:*central-registry*)' \
		--eval '(asdf:load-system :$(PROJECT_BUILDER))' \
		--eval '(format t "✅ $(PROJECT_BUILDER) loaded successfully!~%")' \
		--eval '(format t "📦 Package: $(PROJECT_PACKAGE)~%")' \
		--eval '(in-package :$(PROJECT_PACKAGE))' \
		--eval '(format t "🔹 Available in package: cr8~%")' \
		--eval '(uiop:quit 0)'

clean:
	@echo "🧹 [Project Cleanup]"
	@printf "📝 Enter the project name to delete: "
	@read PROJECT_NAME && \
	if [ -z "$$PROJECT_NAME" ]; then \
		echo "❌ Error: Project name cannot be empty."; \
		exit 1; \
	fi && \
	target_path="$(TARGET_DIR)/$$PROJECT_NAME" && \
	if [ -d "$$target_path" ]; then \
		echo "📁 Found project: $$target_path" && \
		printf "⚠️  Are you sure you want to delete '$$PROJECT_NAME'? (y/n): " && \
		read CONFIRM && \
		if [ "$$CONFIRM" = "y" ] || [ "$$CONFIRM" = "Y" ]; then \
			rm -rf "$$target_path" && \
			echo "✅ Project '$$PROJECT_NAME' deleted successfully."; \
		else \
			echo "🛑 Deletion cancelled."; \
		fi; \
	else \
		echo "❌ Error: Project '$$PROJECT_NAME' not found in $(TARGET_DIR)"; \
		echo "Available projects:"; \
		ls -1 $(TARGET_DIR) 2>/dev/null | head -10 || echo "  (none found)"; \
	fi

lst:
	@echo "📁 List of projects in ~/common-lisp directory:" 
	@echo ""
	@ls -1 $(TARGET_DIR) 2>/dev/null | head -10 || echo \