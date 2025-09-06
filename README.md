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
[Common Lisp Project Generator]

📝 What is the name of the project?: my-project

📂 Creating project: my-project
📁 Target directory: /home/eldriv/common-lisp/my-project

⚡ Generating project structure...
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
[1/16] — /home/eldriv/common-lisp/my-project/README.org
[2/16] — /home/eldriv/common-lisp/my-project/makefile
[3/16] — /home/eldriv/common-lisp/my-project/.gitignore
[4/16] — /home/eldriv/common-lisp/my-project/my-project.asd
[5/16] — /home/eldriv/common-lisp/my-project/my-project-tests.asd
[6/16] — /home/eldriv/common-lisp/my-project/version.sexp
[7/16] — /home/eldriv/common-lisp/my-project/version-tests.sexp
[8/16] — /home/eldriv/common-lisp/my-project/src/main.lisp
[9/16] — /home/eldriv/common-lisp/my-project/src/driver.lisp
[10/16] — /home/eldriv/common-lisp/my-project/src/user.lisp
[11/16] — /home/eldriv/common-lisp/my-project/src/build.lisp
[12/16] — /home/eldriv/common-lisp/my-project/src/utilities.lisp
[13/16] — /home/eldriv/common-lisp/my-project/src/specials.lisp
[14/16] — /home/eldriv/common-lisp/my-project/t/main-tests.lisp
[15/16] — /home/eldriv/common-lisp/my-project/t/driver-tests.lisp
[16/16] — /home/eldriv/common-lisp/my-project/t/user-tests.lisp
✅ Project 'my-project' created successfully!
📂 Location: /home/eldriv/common-lisp/my-project
```
The generated project will look like this:
```
my-project/
├── makefile
├── my-project.asd
├── my-project-tests.asd
├── README.org
├── src
│   ├── build.lisp
│   ├── driver.lisp
│   ├── main.lisp
│   ├── specials.lisp
│   ├── user.lisp
│   └── utilities.lisp
├── t
│   ├── driver-tests.lisp
│   ├── main-tests.lisp
│   └── user-tests.lisp
├── version.sexp
└── version-tests.sexp

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
4. Marie Dependency:  The idea from Marie is to reduce boilerplate and simplify development. It enhances package hygiene by automatically exporting functions, variables, and other definitions unless explicitly told not to.
```lisp
(uiop:define-package #:ai/src/specials
  (:use #:cl
           #:marie))

(defv *default-name* "world")
```
```lisp
(uiop:define-package #:ai/src/main
  (:use #:cl
           #:marie
           #:ai/src/specials))

(def- greet (name)
  (format nil "Hello, ~A!" name))

(def- say-hello ()
  (greet *default-name*))
```
It has two types of defining forms: 
- The Exporting forms like def, defv, defm, etc., will automatically export the symbol from the package 
- The Non-exporting forms like def-, defv-, etc., define internal/private symbols that remain unexported.

## Customizing Your Projects
To tweak the generated files, you can:
- Add custom replacement functions in `specials.lisp` (modify [*table*](https://github.com/eldriv/cl-skel/blob/main/src/templates/specials.lisp)).
- Create new file generators in the relevant modules.
- Update the `create-files` function in [main.lisp](https://github.com/eldriv/cl-skel/blob/main/src/main.lisp) to include additional files.

## Need Help?
- Check the [FiveAM documentation](https://github.com/lispci/fiveam) for testing tips.
- See [Marie](https://github.com/krei-systems/marie)'s [README](https://github.com/krei-systems/marie/blob/main/README.org) for exporting symbols. 
- Explore [ASDF](https://common-lisp.net/project/asdf/) for system configuration.
- Reach out on the project's [GitHub Issues](https://github.com/eldriv/cl-skel/issues) for support or issues.

> NOTE: This is macro semi codebase library, some approaches are written in macros. 
