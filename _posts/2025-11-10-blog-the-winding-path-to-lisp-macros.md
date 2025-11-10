# The Winding Path to Lisp Macros: A Memoize Adventure

I recently had a deep-dive discussion with a developer learning Common Lisp macros. Their journey was a perfect illustration of the "aha!" moments—and the common pitfalls—that everyone encounters when learning this powerful feature.
If you're feeling lost about macros, you're not alone. The key is to stop thinking of them as "super-functions" and start thinking of them as **code that writes code**.
We explored this concept by trying to build a `memoize` macro, a classic Lisp exercise. This is the story of that exploration.
## The Goal: A `memoize` Macro
Our goal was to create a macro `(memoize (my-func 1 2))` that would: 

Check if a result for `(my-func 1 2)` was already stored in a cache. If **yes**, return the stored result. If **no**, compute `(my-func 1 2)`, store the result, and then return it.
### The Tools for the Job
Before we could even start, we needed a cache. A hash table is perfect, but we need one that can use a list like `(my-func 1 2)` as a key.
* **List Keys:** By default, `(list 'a 1)` and `(list 'a 1)` are not the "same" object. To make a hash table compare their *contents*, we must create it with `:test 'equal`. 
```lisp 
(defvar *my-cache* (make-hash-table :test 'equal)) 
```
* **Checking for Keys:** How do we know if a key exists if its value is `NIL`? `gethash` returns **two** values. The second value is a boolean that's `T` if the key was found, and `NIL` if it wasn't. 
```lisp
(multiple-value-bind (value key-found-p) 
   (gethash my-key *my-cache*) 
   (if key-found-p 
   (print "Found!") 
   (print "Not found.")))
```
## The First Attempt (And the Core Problem)
With these tools, a first attempt might look like this:
```lisp 
  ;; --- THIS CODE IS WRONG --- 
  (defmacro memoize ((f &rest args)) 
    ;; Create the cache? 
    (let ((results (make-hash-table :test 'equal))) 
      `(let* ((mkey (list ',f ,@args))) 
          (multiple-value-bind (result foundp) 
            (gethash mkey results) ; <-- How does this see 'results'? 
            (if foundp 
                result 
                (setf (gethash mkey results) 
                      (apply ',f (list ,@args))))))))
```
This code has a fatal flaw, which is the single most important concept in macros: **Compile-Time vs. Runtime.**
The `defmacro` line runs at **compile-time**. The `let` *inside* the macro definition runs at **compile-time**. The code *inside the backquote (`...`)* is the code that will be generated. This code runs at **runtime**.
The `results` variable is created at compile-time and then... vanishes. The runtime code has no idea what `results` is. It will fail with an "unbound variable" error. What if we put the `let` *inside* the backquote?
```lisp 
   ;; --- THIS CODE IS ALSO WRONG --- 
   (defmacro memoize ((f &rest args)) 
     `(let ((results (make-hash-table :test 'equal))) ; <-- Now inside 
        (let* ((mkey (list ',f ,@args))) 
          (multiple-value-bind (result foundp) 
            (gethash mkey results) 
            (if foundp 
                result 
                (setf (gethash mkey results) 
                      (apply ',f (list ,@args))))))))
``` 
This is also wrong. Every time you call `(memoize (foo 1 2))`, it will expand to code that creates a *brand new, empty* hash table, checks it, finds nothing, and then throws it away. The cache is useless.

## The Standard Solution: `defvar`
The cache must exist *outside* the macro, in a persistent, global, runtime-accessible place. This is what `defvar` is for.
This is the correct, standard, and robust solution:
```lisp 
;; 1. Define the cache ONCE, at load time. 
(defvar *memoize-cache* (make-hash-table :test 'equal))

(defmacro memoize ((f &rest args)) 
  ;; 2. The macro generates code that refers to the global cache 
  `(let* ( 
          ;; Evaluate arguments ONCE and store their values 
          (evaluated-args (list ,@args)) 
          ;; Build the key from the function name and arg values 
          (mkey (cons ',f evaluated-args))) 
      (multiple-value-bind (result found) 
         ;; 3. Access the GLOBAL cache by its name 
         (gethash mkey *memoize-cache*) 
         (if found 
             result 
             ;; 4. Modify the GLOBAL cache 
             (setf (gethash mkey *memoize-cache*) 
                   (apply ',f evaluated-args))))))
```
This works perfectly. The macro generates code that *knows how to find* the single, shared `*memoize-cache*`. For 99% of developers, this is the end of the story.

---

## The "Constant Data" Rabbit Hole
But what if we *insist* on creating the cache at compile-time? This led to a fascinating discovery. What about this?
```lisp 
;; --- A clever, but DANGEROUS, experiment --- 
(let* ((results (make-hash-table :test 'equal))) 
  (defmacro memoize ((f &rest args)) 
  `(let* ((evaluated-args (list ,@args)) 
          (mkey (cons ',f evaluated-args))) 
     (multiple-value-bind (result found) 
       ;; Note the COMMA! 
       (gethash mkey ,results) 
       (if found 
           result 
           (setf (gethash mkey ,results) 
                 (apply ',f evaluated-args)))))))
```
This is *very* different. Here's what happens:
1. The `let*` runs **once**, at compile-time. It creates *one* hash table. 
2. The `defmacro` is defined *inside* that `let*`, so it **captures** the `results` variable. 
3. When the macro expands, the comma (` ,results `) tells Lisp: "Stop! Evaluate `results` *right now* and **splice its value** (the literal hash table object) into the code."
4. This actually **works**. Every macro call splices a pointer to the *exact same* hash table. We've created a shared, compile-time cache!

...so why is it bad? Because SBCL will give you this warning:

```
caught WARNING: Destructive function ... called on constant data: #<HASH-TABLE ...> 
``` 
We're not generating code that looks up a *variable* `*memoize-cache*`. We're generating code that looks like this:

`(setf (gethash mkey #<HASH-TABLE...>) ...)`

We are modifying a **literal, constant object** embedded in our code. The compiler is warning us that this is dangerous and strange. This experiment taught us a profound lesson about what the comma (` , `) actually *does*: it splices a *value*, not a *symbol*.

## The Final Challenge: Pure Quasiquote
As a final "style" challenge, we asked: can we rewrite the standard `defvar` solution *without* using `list` or `cons`? Can we use *only* quasiquote (backtick) syntax?

Yeah, it can. the simple, elegant solution is this:
```lisp 
(defvar *memoize-cache* (make-hash-table :test 'equal))

(defmacro memoize ((f &rest args)) 
  ;; This is the standard defvar solution, but rewritten ;; in a "pure quasiquote" style. 
  `(let* ( 
          ;; This is the "pure" replacement for (list ,@args) 
          ;; It generates code like `(,@(list 1 2)) 
          (evaluated-args `(,@,args))
          ;; This is the "pure" replacement for (cons ',f evaluated-args)
          ;; It generates code like (quasiquote ((quote my-func) (unquote-splice (1 2))))
          (mkey `(,',f ,@evaluated-args)))
    (multiple-value-bind (result foundp)
     (gethash mkey *memoize-cache*)
     (if foundp
         result
         (setf (gethash mkey *memoize-cache*)
               (apply ',f evaluated-args))))))
```
I’m not going to walk through the details of how this transformation works—maybe I’ll dedicate another blog post to that later. Here I want to stress two points that may help when you try to understand it:

1. Be crystal-clear about which parts run at **compile-time** and which parts run at **runtime**.
2. Every expansion or evaluation strips away **one level of quasiquote**.

## Conclusion

This journey was a perfect summary of learning macros: 
1. You start with a simple idea. 
2. You're immediately confronted by the **compile-time vs. runtime** split. 
3. You learn the standard, robust solution (`defvar`). You get curious and experiment, leading you down a rabbit hole (`let*` and the "constant data" warning) that teaches you a *deeper* truth about how your code is assembled.  
4. You finally emerge with the tools to write code that is not just correct, but elegant (`quasiquote`). 
5. Don't be afraid to get lost. The "wrong" paths are often where the real learning happens.
