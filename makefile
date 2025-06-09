SHELL := $(shell which bash 2>/dev/null || which sh)
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.ONESHELL:
.SHELLFLAGS := -c
.DELETE_ON_ERROR:

LISP := sbcl
PROJECT_BUILDER := cl-project-builder
PROJECT_PACKAGE := cl-project-builder/src/main
TARGET_DIR := $(HOME)/common-lisp

.PHONY: all project project-name setup help clean

# Default target
all: project

project:
	@echo "====Common Lisp Project Generator===="
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
		printf "Continue? (y/N): "; \
		read CONFIRM; \
		if [ "$$CONFIRM" != "y" ] && [ "$$CONFIRM" != "Y" ]; then \
			echo "🛑 Aborted."; \
			exit 1; \
		fi; \
	fi && \
	echo "🔧 Generating project structure..." && \
	$(LISP) --eval "(ql:quickload :$(PROJECT_BUILDER))" \
		--eval "(in-package :$(PROJECT_PACKAGE))" \
		--eval "(cr8 \"$$PROJECT_NAME\")" \
		--eval "(format t \"✅ Project '$$PROJECT_NAME' created successfully!~%\")" \
		--eval "(format t \"📂 Location: $(TARGET_DIR)/$$PROJECT_NAME~%\")" \
		--eval "(uiop:quit 0)" || { \
		echo "❌ Error: Failed to create project. Make sure $(PROJECT_BUILDER) is available."; \
		exit 1; \
	}

project-name:
	@if [ -z "$(PROJECT)" ]; then \
		echo "❌ Error: PROJECT variable not set. Use: make project-name PROJECT=my-project"; \
		exit 1; \
	fi
	@echo "📂 Creating project: $(PROJECT)"
	@echo "📁 Target directory: $(TARGET_DIR)/$(PROJECT)"
	@if [ -d "$(TARGET_DIR)/$(PROJECT)" ]; then \
		echo "⚠️  Warning: Directory $(TARGET_DIR)/$(PROJECT) already exists."; \
		printf "Continue? (y/N): "; \
		read CONFIRM; \
		if [ "$$CONFIRM" != "y" ] && [ "$$CONFIRM" != "Y" ]; then \
			echo "🛑 Aborted."; \
			exit 1; \
		fi; \
	fi
	@echo "🔧 Generating project structure..."
	@$(LISP) --eval "(ql:quickload :$(PROJECT_BUILDER))" \
		--eval "(in-package :$(PROJECT_PACKAGE))" \
		--eval "(cr8 \"$(PROJECT)\")" \
		--eval "(format t \"✅ Project '$(PROJECT)' created successfully!~%\")" \
		--eval "(format t \"📂 Location: $(TARGET_DIR)/$(PROJECT)~%\")" \
		--eval "(uiop:quit 0)" || { \
		echo "❌ Error: Failed to create project. Make sure $(PROJECT_BUILDER) is available."; \
		exit 1; \
	}

setup:
	@echo "⚙️  Setting up $(PROJECT_BUILDER)..."
	@if [ ! -f "$(PROJECT_BUILDER).asd" ]; then \
		echo "❌ Error: $(PROJECT_BUILDER).asd not found in current directory."; \
		echo "Please run this makefile from the $(PROJECT_BUILDER) root directory."; \
		exit 1; \
	fi
	@$(LISP) --eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :$(PROJECT_BUILDER))" \
		--eval "(format t \"✅ $(PROJECT_BUILDER) loaded successfully!~%\")" \
		--eval "(format t \"📦 Package: $(PROJECT_PACKAGE)~%\")" \
		--eval "(in-package :$(PROJECT_PACKAGE))" \
		--eval "(format t \"🔹 Available in package: cr8~%\")" \
		--eval "(uiop:quit 0)"

# Show help
help:
	@echo "📚 Common Lisp Project Generator"
	@echo "==============================="
	@echo
	@echo "Available targets:"
	@echo "  make (or make project)       - 🚀 Interactive project creation (prompts for name)"
	@echo "  make project-name PROJECT=name - 📂 Create project with specified name"
	@echo "  make setup                   - ⚙️  Setup and test the project builder"
	@echo "  make help                    - 📖 Show this help message"
	@echo
	@echo "Configuration:"
	@echo "  LISP = $(LISP)               - Lisp implementation to use"
	@echo "  TARGET_DIR = $(TARGET_DIR)   - Where projects are created"
	@echo
	@echo "Examples:"
	@echo "  make                        # 🚀 Interactive mode"
	@echo "  make project-name PROJECT=my-awesome-project"
	@echo "  make TARGET_DIR=/path/to/projects project"
