## **Building a Lisp-style `format` in JavaScript, Part 1: The Specification**

Template literals are one of the most beloved features of modern JavaScript. They make string interpolation simple and readable. But what happens when the logic gets complex? What if you need to loop over
data, apply conditional formatting, or choose from multiple text fragments, all within your string definition?

While you can embed complex JavaScript expressions, the template itself can quickly become messy. For decades, languages in the Lisp family have had an incredibly powerful solution: the `format` function.
It's a mini-language dedicated to creating sophisticated, formatted text.

Inspired by its power and elegance, this series will walk through building a `format` template engine in JavaScript. In this first post, we won't write a single line of implementation code. Instead, we will
do something far more important: we will define our **specification**.

#### **Core Principles**

Before diving into features, let's establish two ground rules for our engine.
1.  **A Single, Pure Function:** Our entire engine will be exposed as a single function: `format(formatString, ...args)`.
2.  **It Always Returns a String:** This function is a pure string-builder. It will **never** print to the console or perform any other side effects. This makes it predictable, testable, and easy to compose
    with other code.

#### **The `format` Mini-Language**

The heart of `format` is the `formatString`, which contains plain text mixed with special "directives." A directive always starts with a tilde (`~`) and tells the engine how to process an argument or control
the output.

#### **A Formal Grammar (BNF)**

To be precise about the structure of our `formatString`, we can define its grammar using a simplified Backus-Naur Form (BNF). This notation gives us an unambiguous blueprint for our parser.

```
format-str      ::= ( text | directive )*
text            ::= <any character except '~'>*

directive       ::= "~" ( simple-directive | iteration | conditional )

simple-directive::= "a" | "%" | "~"

iteration       ::= "{" format-str "}"

conditional     ::= [ ":" ] "[" clauses "]"
clauses         ::= format-str ( "~;" format-str )*
```

Now, let's explore what each of these specified directives actually does.

#### **Group 1: The Basics**

These directives handle the most common tasks: inserting values and controlling layout.

*   **`~a` (Aesthetic)**: The general-purpose "print anything" directive. It consumes one argument and converts it to a human-readable string.
    ```javascript
    format("Hello, ~a! Your ID is ~a.", "Alex", 123)
    //=> "Hello, Alex! Your ID is 123."
    ```

*   **`~%` (Newline)**: Inserts a newline character. It consumes no arguments.
    ```javascript
    format("Line 1~%Line 2")
    //=> "Line 1\nLine 2"
    ```

*   **`~~` (Literal Tilde)**: Escapes the tilde, allowing you to print a literal `~` character.
    ```javascript
    format("The directive character is ~~.")
    //=> "The directive character is ~."
    ```

#### **Group 2: Structural Directives**

This is where our engine truly starts to shine, enabling logic and flow control directly within the template.

*   **`~{...~}` (Iteration)**: Consumes one argument (which must be an array) and applies the inner template to each element.
    ```javascript
    const fruits = ["apple", "banana", "cherry"];
    format("Fruits: ~{~a, ~}", fruits)
    //=> "Fruits: apple, banana, cherry, "
    ```

*   **`~[...]` (Conditional by Index)**: Consumes one numeric argument and uses it as a zero-based index to select a clause. Clauses are separated by `~;`.
    ```javascript
    // Selects the clause at index 1
    format("The item is ~[small~;medium~;large~].", 1)
    //=> "The item is medium."
    ```

*   **`~:[...]` (Conditional by Boolean)**: A powerful variant for truthiness. If the argument is `false`, `null`, or `undefined`, it selects the first clause. For any other value (`true`, a number, a
    string, etc.), it selects the second.
    ```javascript
    format("Status: ~:[offline~;online~]", true)
    //=> "Status: online"

    format("User: ~:[guest~;logged in~]", null)
    //=> "User: guest"
    ```

#### **Putting It All Together**

Let's see how these directives can be combined to generate a simple report. Note how the inner directives `~a` and `~:[...]` operate on each `user` object passed into the `~{...~}` loop.

```javascript
const users = [
  { name: "Alice", active: true },
  { name: "Bob", active: false },
  { name: "Charlie", active: true }
];

const report = format("User Report:~%~{~a: ~:[inactive~;active~]~%~}", users);

/*
The final string returned by `report` will be:

"User Report:
Alice: active
Bob: inactive
Charlie: active
"
*/
```
Without any messy string concatenation or `.map().join()`, we've created a complex, multi-line string based on our data. The template clearly expresses the desired output structure.

#### **Further Usage Scenarios**

To further illustrate the flexibility of these directives, let's explore a few more common formatting challenges.

**1. Handling Plurals**

A classic problem: you need to correctly write "1 file" vs. "2 files". The `~:[...]` directive is perfect for this. We can pass it a boolean condition.

```javascript
function fileReport(count) {
  // `count !== 1` is false for 1, but true for 0 or 2+
  return format("Found ~a file~:[~;s~].", count, count !== 1);
}

fileReport(1); //=> "Found 1 file."
fileReport(5); //=> "Found 5 files."
fileReport(0); //=> "Found 0 files."
```
Notice we pass two arguments: `count` is consumed by `~a`, and the result of `count !== 1` is consumed by `~:[...]` to decide whether to add the "s".

**2. Generating HTML Lists**

The iteration directive isn't just for comma-separated values. It's excellent for generating structured markup like HTML.

```javascript
const items = ["First item", "Second item", "Third item"];

format("<ul>~%~{  <li>~a</li>~%~}</ul>", items);

/*
Returns:
"<ul>
  <li>First item</li>
  <li>Second item</li>
  <li>Third item</li>
</ul>"
*/
```

**3. Displaying Optional Data**

Sometimes you only want to show a piece of text if a certain property exists or is true.

```javascript
const user1 = { name: "Jane", isAdmin: true };
const user2 = { name: "John", isAdmin: false };

function userDisplay(user) {
  return format("~a~:[~; (Admin)~]", user.name, user.isAdmin);
}

userDisplay(user1); //=> "Jane (Admin)"
userDisplay(user2); //=> "John"
```
Here, the `~:[...]` directive conditionally adds the "(Admin)" text based on the `isAdmin` flag, keeping the template clean and declarative.

**4. Generating a SQL `IN` Clause**

A very common task is building a SQL query with a variable number of IDs. Using `Array.prototype.map` and `join` works, but it separates the query logic from the values. With `format`, we can define the list
structure directly in the template.

```javascript
const userIds = [101, 102, 105];

// The inner template `~a,` is applied to each ID.
let query = format("SELECT * FROM users WHERE id IN (~{~a,~});", userIds);

// This produces a query with a trailing comma:
// "SELECT * FROM users WHERE id IN (101,102,105,);"

// We can easily clean this up. This is a common pattern when using `format`.
query = query.replace(/,(\s*)\)/, '$1)');

// The final, correct query:
// "SELECT * FROM users WHERE id IN (101,102,105);"
```
*Note: In future versions, we might add directives specifically to handle separators between items to avoid this final cleanup step, but for v1, this is a clear and effective approach.*

#### **Next Up: The Implementation**

With this solid specification in hand, we are ready to start building. In the next post, we will dive into the JavaScript code, writing a parser and interpreter for our `format` language. Stay tuned




    
