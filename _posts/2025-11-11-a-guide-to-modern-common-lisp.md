# A Guide to Modern Common Lisp: Packages, Systems, and Projects

One of the first and biggest hurdles for a new Common Lisp developer is the ecosystem. You hear a half-dozen new terms—package, system, ASDF, Quicklisp—and it's not clear how they fit together.

Let's demystify the entire stack, from the Lisp language itself to the tools that make modern development possible.

## Part 1: Packages (The Lisp Language Feature)

A **package** is a **namespace**. That's it. Its only job is to be a "phone book" for symbols (like variable or function names) to prevent name collisions.

Think about it: what happens if you and a library author both define a function called `memoize`? A package system solves this.

* `defpackage`: The macro you use to create a new package. 
* `in-package`: The declaration you put at the top of a file to switch into your package. 
* `export`: A list of symbols you want to make "public." 
* `use`: A list of other packages you want to *inherit* public symbols from. Almost every package you make will `:use :common-lisp`.

### Accessing Symbols

This is where the colons come in.

* `package:symbol`: Accesses a **public (exported)** symbol. (e.g., `my-project:memoize`). 
* `package::symbol`: Accesses a **private (internal)** symbol. (e.g., `my-project::*my-cache*`). Using this is a bad idea, like accessing a private member in Java or Python.

### Example `package.lisp`

By convention, you define your package in its own file.

```lisp 
;; file: package.lisp 
(defpackage :my-memoize-project 
  (:use :common-lisp) 
  (:export :memoize)) ; Only 'memoize' is public 
```
---

## Part 2: Systems (The ASDF Library)

A **system** is a **build recipe**. It's a collection of files and their dependencies. Its job is to tell Lisp *what* files to load, in *what order*, and *what other libraries* are needed.

The library that handles this is **ASDF (Another System Definition Facility)**, which is bundled with every modern Lisp.

* `.asd file`: You define your system in a file with a `.asd` extension (e.g., `my-memoize-project.asd`). The file name **must** match the system name. 
* `defsystem`: The macro inside your `.asd` file that defines the system. 
* `:depends-on`: A list of other systems (libraries) your project needs. 
* `:components`: A list of your project's files. 
* `:serial t` **: A shortcut that says, "Load the components in the exact order I list them."

### Example `my-memoize-project.asd`

```lisp 
;; file: my-memoize-project.asd
(asdf:defsystem :my-memoize-project 
  :description "My memoize project." 
  :author "You" 
  :license "Proprietary" 
  :version "1.0.0"
  ;; My project needs the bordeaux-threads library :depends-on 
  (:bordeaux-threads)
  ;; Load these files, in this exact order 
  :serial t 
  :components ((:file "package") ; Load package.lisp first 
               (:file "memoize") ; Load memoize.lisp second 
               (:file "main")))  ; Load main.lisp last
```
---

## Part 3: Project Structure (The Directory)

A **project structure** is the **directory layout** on your hard drive. It's a convention that makes it easy for ASDF to find your files.

A simple, modern project looks like this:

```
my-memoize-project/
 ├── my-memoize-project.asd <-- The System definition (build recipe) 
 ├── package.lisp <-- The Package definition (namespace) 
 ├── memoize.lisp <-- Your code 
 └── main.lisp <-- More code 
 ```

### How It All Fits Together

This is the "aha!" moment.

1. **Project** is the folder (`my-memoize-project/`).

2. **System** is the `.asd` file in that folder (the "build recipe").

3. **Package** is defined in `package.lisp` (the "namespace"), which is one of the files the system's recipe tells ASDF to load.

When you run `(asdf:load-system :my-memoize-project)`:

1. **ASDF** finds `my-memoize-project.asd`.

2. It reads the system and sees it needs `:bordeaux-threads`. It loads that first.

3. It sees `:serial t` and `:components`.

4. It loads `package.lisp`. Your package is now defined.

5. It loads `memoize.lisp`. That file probably starts with `(in-package :my-memoize-project)`, and its `defmacro memoize` creates the symbol in your package.

6. It loads `main.lisp`, which can now use the `memoize` macro.

---

## Part 4: Multiple Systems in One Project

Yes, a single project (directory) can, and often does, contain multiple system definitions.

### Why Have Multiple Systems?

The most common reason is to **separate your main code from your tests**. You don't want your main application to depend on your testing library (like `rove` or `fiveam`). By making your tests a separate system, you can load your main code *without* also loading all your testing tools.

### Example Structure

``` 
my-project/ 
 ├── my-project.asd <-- Defines the main system :my-project 
 ├── my-project-tests.asd <-- Defines the test system :my-project-tests 
 ├── package.lisp 
 ├── src/ 
 │ └── main.lisp 
 └── tests/ 
 └── main-tests.lisp 
```

### Example `my-project.asd`

```lisp
(defsystem :my-project 
  :serial t 
  :components ((:file "package") 
               (:file "src/main")))
```

### Example `my-project-tests.asd`

```lisp 
(defsystem :my-project-tests 
  :depends-on (:my-project ; <-- Depends on your main code 
               :rove) ; <-- Depends on a test library 
  :serial t 
  :components ((:file "tests/main-tests")) 
  :perform (test-op (o c) 
             (uiop:symbol-call :rove :run o)))
```

### How You Use It

* **To load your app (e.g., in production):** `(ql:quickload :my-project)`

* **To load your app AND your tests (e.g., for development):** `(ql:quickload :my-project-tests)` (This automatically loads `:my-project` first because it's a dependency)

* **To run your tests:** `(asdf:test-system :my-project)`

---

## Part 5: Quicklisp (The Ecosystem)

So how does ASDF find `:bordeaux-threads`? And how does it find *your* `my-memoize-project.asd` file?

This is where **Quicklisp** comes in. Quicklisp is the **library manager** (like `pip` or `npm`).

* `ql:quickload`: This is the command you use. It finds, downloads, and *then tells ASDF to load* any library in its collection. 
* `~/quicklisp/`: When you run `(quicklisp:install)`, it creates this directory. You can change this path with `(quicklisp:install :path "/my/path/")`.
* `ql:*quicklisp-home*`: This variable holds the path to your Quicklisp directory.
* `~/quicklisp/dist/`: This is the folder where all your dependencies will be putting in.
* `~/quicklisp/local-projects/`: This is the magic folder. Quicklisp automatically tells ASDF, "Hey, in addition to all the libraries I manage, *also* look for systems in this `local-projects/` directory."

This gives you the canonical, simple workflow:

Write your project (e.g., `my-memoize-project/`).

Drop that *entire* folder into `~/quicklisp/local-projects/`.

Start your Lisp and just run: 
```lisp 
(ql:quickload "my-memoize-project")
``` 
That's it. `ql:quickload` unifies loading for *both* internet libraries and *your* local code.

---

## Part 6: Advanced ASDF Configuration

What if you *don't* want to put your projects in `~/quicklisp/local-projects/`? You have to tell ASDF where to look. There are two main ways.

### Method 1: The Old Way (`*central-registry*`)

You can programmatically `push` your project's parent directory onto a global variable in your Lisp startup file (e.g., `~/.sbclrc`). 
```lisp 
;; In ~/.sbclrc 
(push #p"~/my-code/" asdf:*central-registry*) 
``` 
Now ASDF will scan `~/my-code/` for `.asd` files. The only caveat is that the .asd file—named exactly like the system—must sit directly in that directory, not in a subdirectory.

### Method 2: The New Way (`source-registry.conf`)

A cleaner, more modern way is to use ASDF's declarative configuration file.

* **File:** `~/.config/common-lisp/source-registry.conf` 
* **Or (better):** `~/.config/common-lisp/source-registry.conf.d/50-my-projects.conf`

Inside this file, you use a special syntax. The most useful is `(:tree ...)`:

```lisp 
;; In ~/.config/common-lisp/source-registry.conf.d/50-my-projects.conf 
(:source-registry
  (:tree (:home "/home/me/my-code/")
  :inherit-configuration)
``` 
This tells ASDF to **recursively search** the entire `/home/me/my-code/` directory for any `.asd` files it can find.

### Do They Overlap?

No. `*central-registry*` and `source-registry.conf` are **two separate, parallel mechanisms**. Modifying one does not affect the other. 

ASDF simply checks *all* of these places: 
* Paths in `*central-registry*`. 
* Paths in the `source-registry.conf` files. 
* The `~/quicklisp/local-projects/` directory (if Quicklisp is loaded).

### More about `source-registry.conf`
Actually, you don’t need a source-registry.conf at all—you can just add the directory programmatically like this:
```Lisp
(asdf:initialize-source-registry
 '(:source-registry
   (:tree #p"/home/my-code/")
   :inherit-configuration))
```
After execute this code explicitly or load the `source-registry.conf`, you can check the variable `asdf::*source-registry*` to see the result:
```lisp
;; note: asdf::*source-registry* is a map
(let ((ht asdf::*source-registry*))
  (maphash (lambda (k v) 
             (format t "~&~A -> ~A" k v)) ht))
             
;; two function you may be interested
(asdf:clear-source-registry)
(asdf::flatten-source-registry)  
```
That’s not the whole story — I’ve simplified things a bit. But it’s good enough for now.
If you want to dig deeper, you can start here:
* asdf::*system-definition-search-functions*
* asdf:*default-source-registries*

### Can I Change the Config File Location?

Yes. ASDF finds its config by following the XDG Base Directory Specification. 
* It looks for the `XDG_CONFIG_HOME`  environment variable. 
* If `XDG_CONFIG_HOME` is not set, it defaults to `~/.config/`. 
* If you set `export XDG_CONFIG_HOME="/my-configs"` in your `.bashrc`, ASDF will then look for `/my-configs/common-lisp/source-registry.conf`.

### Can I change the Quicklisp local project Location?

Yes. To add a new directory to Quicklisp's local projects path, you must modify the special variable ql:*local-project-directories* directly.
```Lisp
(push #p"~/my-other-lisp-projects/" ql:*local-project-directories*)
```
“Distributions” (dists) are a more complex topic than local projects, so I’ll skip them here and let you explore them on your own. 

Just remember, you can change its location too.
