### **Building a Lisp-style `format` in JavaScript, Part 2: The Implementation**

In [Part 1 of this series](https://misakawasaki.github.io/2025/09/23/blog-build-a-lisp-style-format-specification.html), we laid the groundwork for our project by defining a clear specification for a JavaScript-based, Lisp-inspired `format` function. We defined our core principles
and the exact behavior of our initial set of directives: `~a`, `~%`, `~~`, `~{...~}`, and `~[...~]`.

Now, it's time to bring that specification to life. In this post, we'll dive deep into the implementation, focusing not just on making it work, but on building it in a clean, modular, and extensible way.

#### **Architecting for Extensibility**

It would be easy to write our function with a giant `switch` statement, but that would quickly become a maintenance nightmare as we add more features. Instead, we'll adopt a more robust architecture based on
a few key concepts:

1.  **A Parser State Object:** We'll bundle the entire state of our parsing process—the string, the arguments, our current position, and the result—into a single `state` object. This keeps our function
    signatures clean.
2.  **A Directive Handler Registry:** This is the core of our extensible design. We'll create a simple JavaScript object that maps directive characters (like `'a'` or `'{'`) to the functions that handle
    them. To add a new directive, we'll just add a new entry to this object.
3.  **A Central Engine Loop:** The main `format` function will be a simple loop. Its only job is to walk through the string, find a tilde (`~`), look up the corresponding handler in our registry, and
    delegate the work to it.

This "pluggable" architecture separates the "what" from the "how" and is a powerful pattern for building interpreters and parsers.

#### **A Note on State: The Engine's Logic Flow**

The name `ParserState` is deliberate. Our function is a simple state machine. However, it's not a formal *Finite State Machine (FSM)* with many explicit, named states (e.g., `READING_TEXT`, `SAW_TILDE`).
Instead, our `state` object acts as a single **context object**—a snapshot of the entire process at any given moment.

The logic is a simple loop that makes a decision based on the current character. Here is a diagram illustrating that flow:

```
                      +-------------------+
                      |   Start `format`  |
                      | (Create `state`)  |
                      +-------------------+
                               |
                               v
                      +-------------------+
                      | Loop: Has string  |
                      |   ended? (Y/N)    |----+
                      +-------------------+    |
                          | (No)               | (Yes)
                          v                    |
            +---------------------------+      |
            | Read char at `state.i`    |      |
            | Is it a tilde '~'? (Y/N)  |      |
            +---------------------------+      |
                  | (Yes)        | (No)        |
                  v              v             |
  +--------------------------+  +------------+ |
  | Look up directive char   |  | Append char| |
  |   in `directiveHandlers` |  | to `result`| |
  +--------------------------+  +------------+ |
                  |                  |         |
                  v                  |         |
      +------------------------+     |         |
      | Found? -> Call Handler |-----+         |
      | Not Found? -> Print ~  |               |
      +------------------------+               |
                                               v
                                         +-----------+
                                         | End Loop  |
                                         | Return    |
                                         | `result`  |
                                         +-----------+

```
As the diagram shows, the engine is in one of two main "modes" on each iteration: "appending text" or "handling a directive." The `state` object is passed to the handlers, which can then modify it (e.g., by
consuming arguments or advancing the string pointer) before returning control to the main loop. This is a very pragmatic and effective way to manage the parsing process.

#### **The Code: The Engine and its Helpers**
Let's start with the skeleton of our `format` function and the helper that finds matching delimiters for our structural directives.
```javascript
/**
 * Finds the matching closing delimiter for a structural directive (e.g., ~{...~}).
 * This is crucial for handling nested structures correctly.
 */
function findMatchingDelimiter(state, openChar, closeChar) {
    let depth = 1;
    // Start searching from the character *after* the opening directive
    for (let j = state.i + 1; j < state.str.length; j++) {
        if (state.str[j] === '~') {
            const nextChar = state.str[j + 1];
            if (nextChar === openChar) {
                depth++;
            } else if (nextChar === closeChar) {
                depth--;
                if (depth === 0) {
                    return j; // Found it!
                }
            }
            j++; // Skip the directive character itself
        }
    }
    return -1; // No match found
}

/**
 * The main `format` engine.
 */
function format(formatString, ...args) {
    const state = {
        str: formatString, // The string being parsed
        args: args,        // The arguments list
        i: 0,              // Current index in the format string
        argi: 0,           // Current index in the arguments list
        result: "",        // The accumulated output string
        modifier: null,    // For handling ':' and '@'
    };

    while (state.i < state.str.length) {
        if (state.str[state.i] === '~') {
            state.i++; // Move past the '~'
            state.modifier = null;

            // Check for modifiers like ':'
            if (state.str[state.i] === ':') {
                state.modifier = ':';
                state.i++;
            }

            const char = state.str[state.i];
            const handler = directiveHandlers[char];

            if (handler) {
                handler(state); // Delegate to the registered handler
            } else {
                // Unrecognized directive, so we print it literally.
                state.result += '~';
                if (state.modifier) state.result += state.modifier;
                state.result += char;
            }
        } else {
            state.result += state.str[state.i];
        }
        state.i++;
    }

    return state.result;
}
```
#### **The Directive Handler Registry**
This is where we define the behavior for each directive. Each function takes the `state` object and is responsible for appending to `state.result` and updating the string and argument pointers (`state.i` and
`state.argi`).

```javascript
const directiveHandlers = {
    /** ~a - Aesthetic: Print argument as a human-readable string. */
    'a': (state) => {
        if (state.argi < state.args.length) {
            state.result += String(state.args[state.argi++]);
        }
    },

    /** ~% - Newline. */
    '%': (state) => {
        state.result += '\n';
    },

    /** ~~ - Literal tilde. */
    '~': (state) => {
        state.result += '~';
    },

    /** ~{...~} - Iteration. */
    '{': (state) => {
        const end = findMatchingDelimiter(state, '{', '}');
        if (end === -1) {
            state.result += '~{'; // Unmatched, print literally.
            return;
        }

        const loopBody = state.str.substring(state.i + 1, end);
        const list = state.args[state.argi++];
        if (Array.isArray(list)) {
            // For each item in the list, recursively call `format`.
            // The item itself becomes the sole argument for that execution.
            for (const item of list) {
                state.result += format(loopBody, item);
            }
        }
        // Advance parser past the entire ~{...~} block
        state.i = end + 1;
    },

    /** ~[...]~] and ~:[...]~] - Conditional. */
    '[': (state) => {
        const end = findMatchingDelimiter(state, '[', ']');
        if (end === -1) {
            state.result += '~['; // Unmatched, print literally.
            return;
        }

        const clausesStr = state.str.substring(state.i + 1, end);
        const clauses = clausesStr.split(/~;/g);
        const selector = state.args[state.argi++];
        let clauseIndex = -1;

        // Check if the ':' modifier is active
        if (state.modifier === ':') {
            clauseIndex = (selector === false || selector === null || selector === undefined) ? 0 : 1;
        } else if (typeof selector === 'number') {
            clauseIndex = selector;
        }

        if (clauseIndex >= 0 && clauseIndex < clauses.length) {
            // Recursively format the chosen clause with the rest of the arguments.
            state.result += format(clauses[clauseIndex], ...state.args.slice(state.argi));
        }

        // Advance parser past the entire ~[...]~] block
        state.i = end + 1;
    }
};
```
Notice the beautiful recursion: for both iteration and conditionals, we call `format` again on a substring. This allows our directives to be nested to any depth, for free!

#### **The Final Result**

With all the pieces assembled, we now have a fully functional and extensible `format` engine. It meets all the requirements of our specification and is ready for future enhancements. For instance, adding a
directive for hexadecimal output (`~x`) is now as simple as adding a new three-line function to the `directiveHandlers` object.

This journey shows that by investing a little time in architecture, we can turn a simple tool into a powerful and maintainable engine.

---

### **Get the Full Code**

For those who want to dive in, experiment, and use this engine in their own projects, we've prepared the complete source code. You can find both the extensible JavaScript version discussed in this post, as
well as a fully-typed TypeScript version for enhanced robustness.

*   **JavaScript Implementation:** [format.js](https://gist.github.com/misakawasaki/9b839d3b47ffa53a16d1d767b61c42ba)
*   **TypeScript Implementation:** [not yet]()

Feel free to use them, learn from them, and extend them to fit your needs. Happy formatting
