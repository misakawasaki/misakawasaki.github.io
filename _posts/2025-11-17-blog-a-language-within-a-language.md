# A Language Within a Language: An Introduction to Type-Level Programming in TypeScript

What if I told you that every time you write TypeScript, you're working with two distinct programming languages? The first is obvious: the JavaScript-like runtime code that gets compiled and executed. But the second is hidden in plain sight, a powerful, purely functional language that runs only at compile time. I'm talking about the TypeScript type system itself.

This might sound like a purely academic curiosity, but it's one of the most profound aspects of modern TypeScript. We're not just talking about defining shapes of objects or annotating function signatures. We're talking about writing *programs* that execute within the type checker—programs that can reason, compute, and transform data before a single line of your code ever runs.

This is the world of **type-level programming**, and in this series, we'll explore it not as a collection of advanced features, but as a language in its own right.

## The "Aha!" Moment: Turing-Completeness

The key to this perspective is a concept from computer science: **Turing-completeness**. A system is Turing-complete if it can be used to simulate any single-taped Turing machine. In simpler terms, it means the system is computationally universal—if something can be computed, it can be computed by this system.

Programming languages like JavaScript, Python, and C++ are Turing-complete. But it turns out, so is TypeScript's type system. Through a combination of features like generics, conditional types (`T extends U ? X : Y`), and the `infer` keyword, we have all the building blocks we need:

*   **Variables:** Generic type parameters.
*   **Functions:** Generic types that map input types to output types.
*   **Conditionals:** Conditional types.
*   **Recursion:** The ability for a type to refer to itself.

This means we can perform complex computations—like parsing a string, implementing a state machine, or even calculating a Fibonacci sequence—entirely at the type level. Think of it as a compile-time interpreter that runs a special-purpose functional language.

## So What? Why Bother Programming at the Type Level?

This is a fair question. It's a mind-bending topic, and it's certainly not something you'll need in your day-to-day work building simple applications. But understanding it unlocks a new level of mastery over TypeScript and provides some incredible practical benefits:

1.  **Unprecedented Type Safety:** You can enforce complex invariants and business logic at compile time. Imagine a type that prevents you from creating an invalid state transition in your application, or a function that only accepts a URL string matching a specific route parameter format. If the code compiles, you *know* it's correct in ways that runtime checks can't guarantee.

2.  **Hyper-Expressive APIs:** You can build libraries and APIs that are not just easy to use, but almost impossible to misuse. The type system can guide developers, provide rich autocompletion, and give immediate feedback directly in their editor. This is the magic behind libraries like Zod, tRPC, and TanStack Router.

3.  **Intellectual Curiosity:** Frankly, it's a lot of fun! It challenges you to think about programming in a new light—a declarative, functional paradigm where the "output" is a type, not a value. It's a puzzle that can make you a better, more versatile programmer.

In this series, we're going on a journey to become fluent in this hidden language. We'll start with the basics—the "syntax" and "keywords" of type-level programming. Then, we'll write our first "programs" and build up to more complex and powerful computations.

Ready to look at TypeScript in a completely new way? In Part 2, we'll dive into the core building blocks of our new language.

# Part 2: The Grammar of a Type-Level Language

Welcome back. In Part 1, we established a bold premise: TypeScript's type system is a Turing-complete programming language. To program in any language, you must first understand its grammar—its rules, syntax, and core components.  

This is the grammar of type-level TypeScript.

## Values: The Primitives of Our Language

In JavaScript, values are things like `42`, `"hello"`, and `true`. In our type-level language, **types themselves are the values**.

Our language has two categories of primitive values:

1.  **Set-like Primitives:** These represent an infinite set of possible runtime values.
    *   `string`
    *   `number`
    *   `boolean`
    *   `bigint`
    *   `symbol`
    *   `null`
    *   `undefined`
    *   `any`, `unknown`, `never`

2.  **Literal Primitives:** These represent a single, specific runtime value.
    *   String literals: `"hello"`, `"world"`
    *   Numeric literals: `1`, `42`, `-100`
    *   Boolean literals: `true`, `false`

Just as `42` is a value in JavaScript, the *type* `42` is a value in our type-level language. The relationship between the literal type `42` and the set-like type `number` is one of subtyping, not of value and type. The expression `42 extends number` evaluates to `true`, which proves that the literal type `42` is a subtype of `number`.

## Data Structures: Tuples and Objects

Our language has two primary ways to structure data:

1.  **Object Types:** These are equivalent to `structs` or `records` in other languages. They are collections of key-value pairs, where keys are strings and values are other types (our "values").
    ```/dev/null/example.ts#L1-4
    // A value of an object type
    type Person = {
      name: string;
      age: number;
    };
    ```

2.  **Tuple Types:** These are ordered, fixed-length lists of other values (types).
    ```/dev/null/example.ts#L1-2
    // A value of a tuple type
    type Point = [number, number];
    ```

## Operators: How We Combine and Inspect Values

A language isn't useful without operators to manipulate values.

*   **Union (`|`):** This operator combines two values to create a new value representing the *union* of the two. `string | number` is a new value that can be either a `string` or a `number`.

*   **Intersection (`&`):** This operator combines two values to create a new value representing the *intersection*. For object types, this combines their properties. `{ a: string } & { b: number }` results in `{ a: string; b: number }`.

*   **Keyof (`keyof`):** This operator acts on our data structures. It takes an object type as input and produces a new value: a union of that object's keys.
    ```/dev/null/example.ts#L1-5
    type Person = { name: string; age: number; };

    // The value of `PersonKeys` is the literal union: "name" | "age"
    type PersonKeys = keyof Person;
    ```

*   **Indexed Access (`T[P]`):** This operator is used to look up a value within a data structure. It's similar to bracket notation (`obj[key]`) in JavaScript.
    ```/dev/null/example.ts#L1-5
    type Person = { name: string; age: number; };

    // The value of `NameType` is `string`
    type NameType = Person["name"];
    ```

## Expressions and Control Flow

How do we make decisions? The primary construct for control flow is the **conditional type**. It has a specific, rigid syntax:

`CheckType extends TargetType ? TrueType : FalseType`

This is a single, atomic expression. It's crucial to understand that `extends` is not a standalone operator that produces a `true` or `false` value. Instead, the `CheckType extends TargetType` clause is a special syntactic form that can *only* exist to the left of the `?` in a conditional type. You cannot write `type IsString = string extends any;` on its own.

The entire expression works as a unit:
*   If `CheckType` is assignable to `TargetType`, the expression evaluates to `TrueType`.
*   Otherwise, the expression evaluates to `FalseType`.

Let's see it in action:
```/dev/null/example.ts#L1-4
// `Result1` will be the value `string`
type Result1 = "hello" extends string ? string : number;

// `Result2` will be the value `number`
type Result2 = {} extends string ? string : number;
```

## Functions

How do we create reusable logic? With **generic types**. These are the functions of our language. They take type parameters as inputs and return a new type as an output.

```/dev/null/example.ts#L1-4
// Defines a function named `WrapInObject`
// It takes one argument, `T`
// It returns a new object type value
type WrapInObject<T> = { value: T };

// "Calling" the function with the value `string`
type WrappedString = WrapInObject<string>; // Result: { value: string }
```

Just as `extends` is used in conditional types, it also used to place **constraints** on the input arguments of a function. This ensures that the types passed to our function are of a shape it can work with.

For example, If we write a function that gets the keys of an object, we need to guarantee its input T is actually an object.
```/dev/null/example.ts#L1-4
// This function's argument `T` MUST be a subtype of `object`.
type GetKeys<T extends object> = keyof T;

// ✅ This works, because { a: 1 } is an object.
type A = GetKeys<{ a: 1 }>; // Result: "a"

// ❌ This causes a compile-time error, because `string` is not an object.
type B = GetKeys<string>;
```

## Declaring Variables: The `infer` Keyword

The `infer` keyword is one of the most powerful features in our type-level language. It allows us to introduce a new temporary **variable** within the context of a conditional type's `extends` clause.

Think of it like destructuring with assignment. It attempts to match a type against a structure, and if it succeeds, it "pulls out" a piece of that structure into a new named variable (a new type).

Let's build a utility type, `GetReturnType<T>`, that extracts the return type from a function type.

```/dev/null/example.ts#L1-6
type GetReturnType<T> =
  T extends (...args: any[]) => infer R ? R : never;

// Let's test it
type MyFn = () => string;
type FnReturn = GetReturnType<MyFn>; // Result: string
```

Here's how the `infer` keyword functions as a variable declaration:
1.  We check if `T` matches the structure of a function type: `(...args: any[]) => infer R`.
2.  The `...args: any[]` part matches any arguments.
3.  The `infer R` part is the declaration. It says: "If `T` is a function, declare a new type variable `R` and assign the function's return type to it."
4.  If the match is successful, the expression evaluates to `R`, our newly declared variable.
5.  If the match fails (e.g., `T` is not a function), it evaluates to `never`.

## Iteration: Recursion and Mapped Types

Our type-level language does not have `for` or `while` loops. Instead, we perform iteration using two primary constructs: **recursion** for conditional, repeated operations, and **mapped types** for transforming object structures.

### 1. Recursion

A type function can call itself. This allows us to create recursive loops that continue until a base case is met in a conditional type. This is essential for operating on data structures of arbitrary depth or length.

Let's write a function `DeepUnwrap<T>` that recursively unwraps nested `Promise` types until it finds a non-Promise value.

```/dev/null/example.ts#L1-7
type DeepUnwrap<T> =
  T extends Promise<infer U>
    ? DeepUnwrap<U> // Recursive step: call self with the inner type
    : T;            // Base case: T is not a Promise, so return it.

// Test it
type A = DeepUnwrap<Promise<Promise<string>>>; // Result: string
```

### 2. Mapped Types

Mapped types provide a way to "loop" over every property in an object type, creating a new object type based on the original. The syntax `{ [K in keyof T]: ... }` is the equivalent of a `for...in` loop for object keys.

Let's create a function `ToBooleans<T>` that takes an object type and transforms all of its properties into type `boolean`.

```/dev/null/example.ts#L1-7
type ToBooleans<T extends object> = {
  [Property in keyof T]: boolean;
};

type MyObject = { a: number; b: string; };
type MyObjectOfBools = ToBooleans<MyObject>;
// Result: { a: boolean; b: boolean; }
```

## Putting It All Together

With this formal grammar, we can now see the structure of a type-level program more clearly.

*   **Values:** The types themselves (e.g., `string`, `true`, `{}`).
*   **Operators:** `|`, `&`, `keyof`.
*   **Expressions:** Conditional types (`... extends ... ? ... : ...`).
*   **Iteraters:** Mapped types (`{ [K in keyof T]: ... }`).
*   **Functions:** Generic types (`type F<T> = ...`).

We also have more advanced syntax like the `infer` keyword, which lets us declare new variables inside an expression, and recursion/mapped types for iteration. We'll explore those as we start building.

## Up Next

We've defined the grammar. Now it's time to write some code. In Part 3, we will use these building blocks—values, operators, expressions, and functions—to write our first real type-level programs.


# Part 3: Writing Our First Type-Level Programs

In Part 1, we introduced our "language within a language." In Part 2, we defined its formal grammar: values, data structures, operators, and functions. Now, it's time to become programmers in this new paradigm. We will write our first programs by building utility types from scratch.

These "programs" are functions that take types as input and produce new types as output, performing computation entirely at compile time.

## Program 1: An Equality Checker

One of the most fundamental operations in any language is checking for equality. Let's write a type-level function, `Equals<A, B>`, that takes two type values and returns the literal type `true` if they are exactly the same, and `false` otherwise.

Our first instinct might be to use a simple conditional type:

```/dev/null/example.ts#L1-2
// A first, flawed attempt
type Equals<A, B> = A extends B ? true : false;
```

This seems plausible, but it fails because `extends` checks for assignability, not equality. For example, `string` is assignable to `string | number`, but they are not equal types.

```/dev/null/example.ts#L1-2
type A = Equals<string, string | number>; // Result: true. This is not what we want.
```

To achieve true equality, we must check for assignability in both directions. If `A` is assignable to `B` AND `B` is assignable to `A`, they are very likely equal. We can express this "AND" logic with nested conditional types.

```/dev/null/example.ts#L1-10
// A better, but still not perfect, implementation
type Equals<A, B> =
  A extends B
    ? (B extends A ? true : false)
    : false;

// Let's test it
type R1 = Equals<string, string>;             // true
type R2 = Equals<string, number>;             // false
type R3 = Equals<string, string | number>;    // false (Correct!)
type R4 = Equals<"a" | "b", "b" | "a">;       // true (Correct!)
```

This version is much more robust and handles most common cases correctly. It forms the basis of a solid mental model for type equality. (Note: This implementation has edge cases around `any` and `never`, but the official, more complex solutions are built upon this foundational logic).

## Program 2: Accessing Tuple Elements

Let's write a function that operates on one of our core data structures: the tuple. Our program, `First<T>`, will take a tuple type `T` and return the type of its first element.

To do this, we need to:
1.  Ensure our input `T` is a tuple (or array). We'll use a generic constraint: `T extends any[]`.
2.  "Pattern match" on the structure of the tuple to extract the first element. This is the perfect job for the `infer` keyword.

```/dev/null/example.ts#L1-10
// A function to get the first element of a tuple type
type First<T extends any[]> =
  T extends [infer Head, ...any[]]
    ? Head
    : never;

// Let's test it
type T1 = ["a", "b", "c"];
type R1 = First<T1>; // Result: "a"

type T2 = [1, 2, 3];
type R2 = First<T2>; // Result: 1
```

Let's break down the expression `T extends [infer Head, ...any[]] ? Head : never`:
*   **`T extends [ ... ]`**: We check if `T` has the structure of a tuple.
*   **`infer Head`**: This is the magic. It says, "If `T` matches this structure, declare a new type variable `Head` and put the type of the first element into it."
*   **`...any[]`**: This is a rest element. It means we acknowledge there might be other elements after the head, but we don't care what they are.
*   **`? Head`**: If the pattern matches, our function returns the `Head` type we just inferred.
*   **`: never`**: If the pattern does not match (for example, if `T` is an empty tuple `[]`), we return `never`, the "impossible" type, indicating failure.

## Program 3: String Manipulation

Our type-level language can even work with string literal types. Let's create a function, `StartsWith<Str, Prefix>`, that checks if a string type `Str` starts with a specific `Prefix`.

This program will look very similar to our `First` implementation, but for strings.

```/dev/null/example.ts#L1-10
// A function to check for a string prefix
type StartsWith<Str extends string, Prefix extends string> =
  Str extends `${Prefix}${string}`
    ? true
    : false;

// Let's test it
type R1 = StartsWith<"Hello World", "Hello">; // Result: true
type R2 = StartsWith<"Hello World", "World">; // Result: false
type R3 = StartsWith<"abc", "a">;             // Result: true
```

Here, we're using another feature: **template literal types**.
*   The expression `${Prefix}${string}` constructs a new string literal type pattern.
*   It says: "Match any string that starts with the literal type `Prefix`, followed by any other `string` content."
*   If `Str` is assignable to this pattern, the condition is met, and we return `true`.

## Up Next

We've successfully written our first three programs! We've seen how to combine generic constraints, conditional types, `infer`, and template literal types to perform meaningful computation.

In Part 4, we'll tackle more advanced concepts like iteration and recursion by building type-level programs that can "loop" and operate on complex data structures.


# Part 4: Iteration with Recursion and Mapped Types

Welcome to Part 4. We've mastered the basic grammar and written simple, single-shot programs. Now, we'll explore how our type-level language handles one of the most critical concepts in programming: iteration. Without `for` or `while` loops, we rely on two powerful constructs: **recursion** and **mapped types**.

## Program 4: Flattening a Nested Array with Recursion

In traditional programming, you might flatten a nested array using a recursive function that processes an array and calls itself on any sub-arrays it finds. We do the exact same thing at the type level.

Our goal is to write `Flatten<T>`, a function that takes a type like `[1, [2, 3], [4, [5]]]` and produces `[1, 2, 3, 4, 5]`.

This program requires a clear base case and a recursive step:
1.  **Recursive Step:** If the array has a `Head` and a `Tail`, we process the `Head`. If `Head` is an array itself, we recursively `Flatten` it and concatenate the result with the flattened `Tail`.
2.  **Base Case:** If the array is empty, we return an empty array `[]`.

```/dev/null/example.ts#L1-12
type Flatten<T extends any[]> =
  T extends [infer Head, ...infer Tail]
    ? [
        ...(Head extends any[] ? Flatten<Head> : [Head]),
        ...Flatten<Tail>
      ]
    : [];

// Let's test it
type Nested = [1, [2, 3], [4, [5]]];
type Flat = Flatten<Nested>; // Result: [1, 2, 3, 4, 5]

type Empty = Flatten<[]>; // Result: []
```

This is the most complex program we've written, so let's break it down:
*   `T extends [infer Head, ...infer Tail]`: We use `infer` to destructure the tuple `T` into its first element, `Head`, and the rest of its elements, `Tail`.
*   `? [ ... ] : []`: If the destructuring succeeds (the array is not empty), we enter the main body. If not, we've hit our base case (`T` is `[]`), and we return `[]`.
*   `...(Head extends any[] ? Flatten<Head> : [Head])`: This is the core logic. We check if `Head` is an array.
    *   If it is, we make a **recursive call**: `Flatten<Head>`.
    *   If it's not, we wrap it in a tuple `[Head]` so it can be spread.
*   `...Flatten<Tail>`: We then recursively call `Flatten` on the `Tail` to process the rest of the array. The `...` spread syntax works on tuple types, allowing us to concatenate the results into our final, flat tuple.

## Program 5: Creating Setters with Mapped Types

Mapped types are our `for...in` loop for object keys. They excel at transforming one object shape into another.

Let's write a program, `CreateSetters<T>`, that takes an object type like `{ name: string; age: number; }` and creates a new object type with corresponding setter methods: `{ setName: (val: string) => void; setAge: (val: number) => void; }`.

This requires us to:
1.  Iterate over each `Property` in `keyof T`.
2.  Create a new key by capitalizing the property name and prepending "set".
3.  Create a new value, which is a function type that accepts an argument of the original property's type.

We can achieve this by combining **mapped types** with **template literal types**.

```/dev/null/example.ts#L1-12
type CreateSetters<T extends object> = {
  [Property in keyof T as `set${Capitalize<string & Property>}`]:
    (value: T[Property]) => void;
};

// Let's test it
type MyState = {
  name: string;
  age: number;
  active: boolean;
};

type MySetters = CreateSetters<MyState>;
/* Result:
{
  setName: (value: string) => void;
  setAge: (value: number) => void;
  setActive: (value: boolean) => void;
}
*/
```

Let's analyze the new syntax:
*   `[Property in keyof T as \`set\${...\}\`]` This is called **key remapping**. The `as` clause lets us create a completely new key for the property instead of just using the original one.
*   `Capitalize<string & Property>`: `Capitalize` is a built-in type function that upper-cases the first letter of a string type. We use `string & Property` to satisfy its constraint that the input must be a `string`.
*   `(value: T[Property]) => void`: For the value, we create a function type. The `value` parameter's type is looked up from the original object using `T[Property]`, ensuring type safety.

## Up Next

We have now explored the full range of fundamental concepts in type-level programming, from basic values and operators to complex iteration with recursion and mapped types. We've seen that we can manipulate not just types, but the very *shape* of types, in powerful and predictable ways.

While this series has covered the "how," we've only scratched the surface of the "why." In future posts, we could explore practical applications, such as building type-safe validators, parsers, or even state machines, all within the TypeScript type system. The possibilities are truly limitless.

