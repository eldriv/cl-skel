# Common Lisp Project Builder

A fast and flexible tool to generate organized Common Lisp projects — with proper ASDF definitions, testing framework integration, and a standardized directory layout.
---

## Features
- **Automated Project Structure**  
  Generates `src/` and `t/` directories with a clean layout.
- **ASDF Integration**  
  Creates both main and test system definitions.
- **Git Integration**  
  Detects Git username and email for project metadata.
- **Template System**  
  Flexible string-replacement system for customizing generated files.
- **Package Management**  
  Uses package-inferred systems for clean dependency management.
- **Testing Ready**  
  Sets up the [FiveAM](https://github.com/lispci/fiveam) testing framework and test runners.
- **Build System**  
  Includes Makefiles and build scripts for easy compilation.
---

## Dependencies

- [UIOP](https://quickref.common-lisp.net/uiop.html) — portability layer (part of ASDF)  
- [Marie](https://github.com/krei-systems/marie/tree/main) — utility library  
- [FiveAM](https://github.com/lispci/fiveam) — unit testing framework  
---

## Installation

1. Clone this repository or download the project.
2. Load it into your Lisp environment:

    ```lisp
    (ql:quickload :cl-project-builder)
    ```

---

## 🛠️ Usage

### Basic Project Creation

To create a project in the default location (`~/common-lisp/`):

```bash
sbcl
(cl-project-builder/src/main:create-project^cr8 "my-awesome-project")
;; or
(cl-project-builder/src/main:cr8 "my-project")
```

To create a project in a specific directory:

```bash
(cl-project-builder/src/main:create-project^cr8 "my-project" :target "/my/specified/path")
;; or
(cl-project-builder/src/main:cr8 "my-project" :target "/my/specified/path")
```
---

### Generated Project Structure

```plaintext
├── Makefile
├── my-project.asd
├── my-project-tests.asd
├── README.org
├── src
│   ├── build.lisp
│   ├── driver.lisp
│   ├── main.lisp
│   └── user.lisp
├── t
│   ├── driver-tests.lisp
│   ├── main-tests.lisp
│   └── user-tests.lisp
├── version.sexp
└── version-tests.sexp
```
---

### Working with Generated Projects

To load and run your new project:

```bash
sbcl
(ql:quickload :my-project)
(in-package :my-project/src/main)
(hello)
```

Or use the generated `Makefile`:

```bash
cd my-project
make        # Builds the project
make clean  # Cleans build artifacts
```
---

## Customization

You can extend the template system by:

1. Adding new replacement functions to `*table*` in `specials.lisp`.
2. Creating new file generators in the appropriate modules.
3. Modifying `create-files` function under main.lisp to include additional files.
---

