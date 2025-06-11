# Overview
CL Skel is a tool to quickly set up organized Common Lisp projects with a clean structure, ASDF system definitions, and built-in testing. Whether you're starting a new project or prototyping, this tool streamlines the process so you can focus on coding.

## What It Does
- **Sets Up Project Structure**: Creates `src/` and `t/` directories for your code and tests.
- **ASDF Support**: Generates system definitions for your project and tests.
- **Git Integration**: Automatically pulls your Git username and email for project metadata.
- **Customizable Templates**: Uses a flexible system to tailor generated files.
- **Package Management**: Organizes dependencies with package-inferred systems.
- **Testing Ready**: Includes the [FiveAM](https://github.com/lispci/fiveam) testing framework with test runners.
- **Build Tools**: Comes with a `Makefile` for easy compilation and cleanup.

## Prerequisites
You'll need these libraries:
- [UIOP](https://quickref.common-lisp.net/uiop.html) 
- [Marie](https://github.com/krei-systems/marie/tree/main) (Utility library)
- [FiveAM](https://github.com/lispci/fiveam) (for Unit testing)

## Installation
1. Clone or download this repository:
   ```bash
   $ git clone https://github.com/eldriv/cl-skel.git
   ```
2. Load it in your Lisp environment:
   ```lisp
   CL-USER> (ql:quickload :cl-skel)
   ```

## Get started
### From the Lisp REPL
Create a project in the default location (`~/common-lisp/`):
```lisp
CL-USER> (ql:quickload :cl-skel)
CL-USER> (cl-skel/src/main:cr8 "my-project")
#P"/home/hostname/common-lisp/my-project"
```

Specify a custom directory:
```lisp
CL-USER> (cl-skel/src/main:cr8 "my-project" :target "/path/to/your/directory")
```

### From the Terminal
Launch SBCL and run:
```bash
$ sbcl
* (ql:quickload :cl-skel)
* (cl-skel/src/main:cr8 "my-project")
```
Or with a custom path:
```bash
sbcl
* (cl-skel/src/main:cr8 "my-project" :target "/path/to/your/directory")
```

### Using Makefile
```make
$ make
====Common Lisp Project Generator====

📝 What is the name of the project?: my-project

📂 Creating project: my-project
📁 Target directory: /home/hostname/common-lisp/my-project

🔧 Generating project structure...
This is SBCL 2.4.10, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "cl-skel":
  Load 1 ASDF system:
    cl-skel
; Loading "cl-skel"
.
✅ Project 'my-project' created successfully!
📂 Location: /home/hostname/common-lisp/my-project

```
The generated project will look like this:
```
my-project/
├── Makefile          # Build and manage your project
├── my-project.asd    # ASDF system definition
├── my-project-tests.asd  # Test system definition
├── README.org        # Project documentation
├── src/              # Source code
│   ├── build.lisp    # Build script
│   ├── driver.lisp   # Core functionality
│   ├── main.lisp     # Entry point
│   └── user.lisp     # User-defined code
├── t/                # Tests
│   ├── driver-tests.lisp  # Tests for driver
│   ├── main-tests.lisp   # Tests for main
│   └── user-tests.lisp   # Tests for user code
├── version.sexp      # Project version
└── version-tests.sexp  # Test version
```
If you want to delete the newest generated folder project under `~/common-lisp`

``` bash
$ make clean
🧹 [Project Cleanup]
📝 Enter the project name to delete: my-project
📁 Found project: /home/eldriv/common-lisp/my-project
⚠️  Are you sure you want to delete 'my-project'? (y/n): y
✅ Project 'my-project' deleted successfully.

$ make lst
📁 List of projects in ~/common-lisp directory:

adz
asdf
cl-skel
..
..
..
```

## Working with the generated project
1. Load your project:
   ```lisp
   (ql:quickload :my-project)
   (in-package :my-project/src/main)
   (hello)  ;; Try the sample function
   ```

2. Run tests using FiveAM:
   ```lisp
   (ql:quickload :my-project)
   (asdf:test-system :my-project)
   ```

3. Build or clean with the `Makefile`:
   ```bash
   make        # Compile the project
   make clean  # Remove build files
   ```

## Customizing Your Projects
To tweak the generated files, you can:
- Add custom replacement functions in `specials.lisp` (modify [*table*](https://github.com/eldriv/cl-skel/blob/main/src/templates/specials.lisp)).
- Create new file generators in the relevant modules.
- Update the `create-files` function in [main.lisp](https://github.com/eldriv/cl-skel/blob/main/src/main.lisp) to include additional files.

## Need Help?
- Check the [FiveAM documentation](https://github.com/lispci/fiveam) for testing tips.
- Explore [ASDF](https://common-lisp.net/project/asdf/) for system configuration.
- Reach out on the project's [GitHub Issues](https://github.com/eldriv/cl-skel/issues) for support or issues.

> NOTE: This is macro semi codebase library, some approaches are written in macros. 
