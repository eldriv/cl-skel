# cl-skel

**cl-skel** scaffolds a small Common Lisp application: ASDF systems, `src/` and `t/` trees, a sample `hello` entrypoint, and a minimal test runner. It is meant for quick starts and experiments, not as a full application framework.

Everything runs on **SBCL** (or any Lisp that already ships **ASDF 3** and **UIOP**) plus **GNU Make** if you want the convenience targets. There is no Quicklisp dependency; loading is plain `(require "asdf")` and `asdf:load-system`.

The Lisp sources are written for **ASDF/UIOP** and behave the same on **Linux**, **macOS**, and anywhere else SBCL runs. The **Makefile** is written for a POSIX shell (typical on Linux and macOS). On **Windows**, use **WSL2**, **Git Bash + GNU Make**, or run SBCL by hand from a REPL instead of `make`.

---

## Prerequisites

- **SBCL** (or another Lisp that ships **ASDF 3** and **UIOP**) — required. Normal SBCL installs qualify. This is not “ANSI CL only”: ASDF/UIOP come with the implementation, not from Quicklisp.
- **GNU Make** — optional; only if you use Makefile targets (`make`, `make setup`, `make project-name`, …). You can load and run **cl-skel** from a REPL without Make.
- **Quicklisp / Ultralisp** — not used. No extra Lisp libraries to install for **cl-skel** or generated projects.
- **Git** — optional; templates read `git config user.name` / `user.email` when present, otherwise placeholders are fine.

If `sbcl` is not on your `PATH`, pass the full path to Make, for example:

- Linux (distro package): `make setup LISP=/usr/bin/sbcl`
- macOS (Homebrew): `make setup LISP=/opt/homebrew/bin/sbcl`
- Any OS: `make setup LISP=/path/where/you/installed/sbcl`

With `LISP` unset, the Makefile uses `command -v sbcl`, then checks `/usr/bin/sbcl`, `/usr/local/bin/sbcl`, and `/opt/homebrew/bin/sbcl` in that order.

---

## What gets generated

By default, `cr8` writes under `~/common-lisp/<name>/` (the usual `user-homedir-pathname` convention). On Windows, or if you prefer another root, pass `:target` to `cr8` with a pathname your Lisp accepts (for example `#p"C:/Users/you/projects/"`). A project named `my-project` looks like this:

```
my-project/
├── makefile
├── my-project.asd
├── my-project-tests.asd
├── README.org
├── src
│   ├── build.lisp
│   ├── driver.lisp
│   ├── main.lisp
│   ├── specials.lisp
│   ├── user.lisp
│   └── utilities.lisp
├── t
│   ├── driver-tests.lisp
│   ├── main-tests.lisp
│   └── user-tests.lisp
├── version.sexp
└── version-tests.sexp
```

Systems use **package-inferred-system** so each file’s package matches its path (for example `my-project/src/main`). Generated tests use `assert` and a tiny `run-tests` hook wired to ASDF’s `test-op`.

---

## Clone and load this repository

```bash
git clone https://github.com/eldriv/cl-skel.git
cd cl-skel
```

Register the checkout on ASDF’s central registry, then load:

```lisp
(require "asdf")
(push #P"/absolute/path/to/cl-skel/" asdf:*central-registry*)
(asdf:load-system :cl-skel)
```

From the repo root, `make setup` runs the same idea in one shot (SBCL non-interactive, push cwd, load `:cl-skel`).

To run **cl-skel’s own tests**:

```bash
sbcl --non-interactive \
  --eval '(require "asdf")' \
  --eval '(push (uiop:ensure-directory-pathname (uiop:getcwd)) asdf:*central-registry*)' \
  --eval '(asdf:test-system :cl-skel)' \
  --eval '(uiop:quit 0)'
```

Use your real `sbcl` path if it is not on `PATH`.

---

## Creating a project

**From the shell** (stay in the cl-skel repo so ASDF sees `cl-skel.asd`):

```bash
make                              # prompts for a name
make project-name PROJECT=acme    # non-interactive
```

**From a Lisp REPL** (after loading `:cl-skel` as above):

```lisp
(cl-skel/src/main:cr8 "acme")
;; => #P".../common-lisp/acme/"   (default parent ~/common-lisp/)

(cl-skel/src/main:cr8 "acme" :target #P"/tmp/projects/")
;; parent directory of your choice
```

The first argument is the project name; it becomes the directory name and the primary ASDF system designator (e.g. `:acme`).

Other useful targets: `make clean` (remove a project under the configured parent, with prompts), `make lst` (list that parent).

---

## Working inside a generated project

Point ASDF at the **directory that contains** `<name>.asd`, then load and test. The system keyword always matches the project name (`:acme` for folder `acme`).

```lisp
(require "asdf")
(push #P"~/common-lisp/acme/" asdf:*central-registry*)
(asdf:load-system :acme)
(in-package :acme/src/main)
(hello)

(asdf:test-system :acme)
```

The generated **makefile** loads ASDF from the project root, pushes the cwd onto the registry, and runs `asdf:make` on that system—useful when you do not want a REPL.

**One-shot smoke test from the shell** (adjust path and `:system` to match what you actually created):

```bash
sbcl --non-interactive \
  --eval '(require "asdf")' \
  --eval '(push (uiop:ensure-directory-pathname #P"'"$HOME"'/common-lisp/acme/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :acme)' \
  --eval '(asdf:test-system :acme)' \
  --eval '(uiop:quit 0)'
```

If ASDF says a component is missing, almost always the registry path or the keyword does not match the project you generated (for example loading `:smoke-test-proj` while only `acme/` exists).

---

## Changing what gets emitted

Templates and wiring live under `src/templates/` and `src/main.lisp` (`create-files`). Substitution markers and git-backed defaults are in `src/templates/specials.lisp` (`*table*`, git user placeholders). Adjust those, or add new `deftemplate` definitions and list the new outputs in `create-files`.

---

## Further reading

- [ASDF](https://common-lisp.net/project/asdf/) — system definitions and `test-op`.
- [Issues](https://github.com/eldriv/cl-skel/issues) — bugs and questions for this repo.
