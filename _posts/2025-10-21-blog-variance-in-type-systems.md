# Variance in Type Systems

A comprehensive guide to understanding variance, covariance, contravariance, and invariance in programming language type systems.

---

## Table of Contents

1. [What is Variance?](#what-is-variance)
2. [Understanding Type Constructors](#understanding-type-constructors)
3. [The Four Types of Variance](#the-four-types-of-variance)
4. [Functions and Variance](#functions-and-variance)
5. [Why Variance Matters](#why-variance-matters)
6. [Java's Approach to Variance](#javas-approach-to-variance)
7. [Comparison Across Languages](#comparison-across-languages)

---

## What is Variance?

**Variance** is a property that describes **how a type constructor behaves with respect to subtyping**.

In simpler terms: When you have a subtyping relationship between two types, variance determines what happens to that relationship when those types are used as parameters to type constructors.

### The Core Question

Given that `Dog <: Animal` (Dog is a subtype of Animal), variance answers:

**What's the relationship between `List<Dog>` and `List<Animal>`?**

Possible answers:
- A. `List<Dog> <: List<Animal>` (covariance - same direction)
- B. `List<Animal> <: List<Dog>` (contravariance - reversed)
- C. No relationship (invariance - unrelated)
- D. Both directions (bivariance - both work)

**Variance is the rule that determines which answer is correct for each type constructor.**

---

## Understanding Type Constructors

A **type constructor** is something that takes one or more types and produces a new type:

```
List<T>        - takes a type T, gives you List<T>
Array<T>       - takes a type T, gives you Array<T>
Function<A, R> - takes types A and R, gives you Function<A, R>
Optional<T>    - takes a type T, gives you Optional<T>
```

These are "constructors" because they "construct" new types from existing types.

---

## The Four Types of Variance

### 1. Covariance (↓ preserves subtyping order)

If `Dog <: Animal`, then `Container<Dog> <: Container<Animal>`.

**The subtyping relationship is preserved in the same direction.**

#### When It's Safe

Covariance is safe for **read-only** operations (producers).

#### TypeScript Example

```typescript
class Animal { }
class Dog extends Animal { }

// Arrays are covariant in their read operations
let animals: Animal[] = [];
let dogs: Dog[] = [new Dog()];

animals = dogs; // ✅ OK - covariance allows this
// You can treat a list of Dogs as a list of Animals (for reading)
```

**Why it's safe for reading:** If you expect an `Animal` and get a `Dog`, that's fine - a `Dog` IS an `Animal`.

---

### 2. Contravariance (↑ reverses subtyping order)

If `Dog <: Animal`, then `Container<Animal> <: Container<Dog>`.

**The subtyping relationship is reversed.**

#### When It's Safe

Contravariance is safe for **write-only** operations (consumers).

#### Example with Function Parameters

```typescript
// Function parameters are contravariant
type AnimalHandler = (animal: Animal) => void;
type DogHandler = (dog: Dog) => void;

let handleAnimal: AnimalHandler = (a) => { console.log("Handling animal"); };
let handleDog: DogHandler = (d) => { console.log("Handling dog"); };

// Contravariance in action:
handleDog = handleAnimal; // ✅ OK in languages with proper contravariance
// A function that can handle ANY animal can surely handle a Dog

// But this is NOT safe:
// handleAnimal = handleDog; // ❌ ERROR
// A function that only knows how to handle Dogs can't handle all Animals
```

**Why it's safe:** If you need a function that accepts `Dog`, you can use a function that accepts `Animal` - it will work for any `Dog` you pass.

---

### 3. Invariance (= no subtyping relationship)

Even if `Dog <: Animal`, there's **no relationship** between `Container<Dog>` and `Container<Animal>`.

#### When It's Necessary

Invariance is necessary for **mutable containers** that support both reading and writing.

#### Java Example

```java
// Java generics are invariant by default
List<Animal> animals = new ArrayList<>();
List<Dog> dogs = new ArrayList<>();

// animals = dogs; // ❌ ERROR - invariant, no relationship

// Why? Consider if this were allowed:
// animals.add(new Cat()); // We'd be adding a Cat to a list of Dogs! 💥

// Invariance prevents both:
// - Covariant problems (adding wrong types)
// - Contravariant problems (getting wrong types when reading)
```

**Why it's necessary:** Mutable containers need both read and write operations. Covariance breaks writes, contravariance breaks reads, so we need invariance.

---

### 4. Bivariance (↕ both directions - rarely used)

Both `Container<Dog> <: Container<Animal>` AND `Container<Animal> <: Container<Dog>`.

**This is generally unsafe and rarely intentional.** TypeScript method parameters historically had this issue, but it's mainly considered a bug/compromise for JavaScript compatibility.

---

## Quick Reference Table

| Variance Type | Subtyping Rule | Common Use Cases |
|--------------|----------------|------------------|
| **Covariant** | `Dog <: Animal` ⟹ `F<Dog> <: F<Animal>` | Return types, read-only collections, producers |
| **Contravariant** | `Dog <: Animal` ⟹ `F<Animal> <: F<Dog>` | Function parameters, consumers |
| **Invariant** | No relationship | Mutable collections, read-write containers |
| **Bivariant** | Both directions (unsafe) | Avoid (legacy compatibility issues) |

---

## Functions and Variance

Functions are special type constructors because they have **multiple type positions**, and each position has **different variance**.

### Function Type Structure

```
Function type:  (A) => R

This is really:  Function<A, R>

A = Argument type position
R = Return type position
```

**Key insight:** Different positions in the same type constructor can have different variance!

### Return Type is Covariant

```typescript
type AnimalProducer = () => Animal;
type DogProducer = () => Dog;

// Since Dog <: Animal
// A function that returns Dog can be used where we expect a function that returns Animal

let animalProducer: AnimalProducer;
let dogProducer: DogProducer = () => new Dog();

animalProducer = dogProducer;  // ✅ SAFE!

// Why? When I call animalProducer(), I expect an Animal
// Getting a Dog is perfectly fine - Dog IS AN Animal
```

**Rule:** Return types are covariant because producers are covariant. If you promise to give me an Animal and give me a Dog instead, that's safe.

### Parameter Type is Contravariant

```typescript
type AnimalConsumer = (a: Animal) => void;
type DogConsumer = (d: Dog) => void;

// Since Dog <: Animal
// A function that accepts Animal can be used where we expect a function that accepts Dog

let dogConsumer: DogConsumer;
let animalConsumer: AnimalConsumer = (a) => console.log(a.name);

dogConsumer = animalConsumer;  // ✅ SAFE!

// Why? When I call dogConsumer(new Dog()), 
// the animalConsumer function can handle it - it handles ANY Animal
// Dogs are Animals, so it works!

// But the REVERSE is not safe:
// animalConsumer = dogConsumer;  // ❌ UNSAFE!
// Because we might call animalConsumer(new Cat()), and dogConsumer can't handle Cats
```

**Rule:** Parameter types are contravariant because consumers are contravariant. If you need a function that can handle Dogs, one that can handle any Animal works.

### Complete Function Example

```typescript
// Function type: (Param) => Return

class Animal { }
class Dog extends Animal { }
class Cat extends Animal { }

type F1 = (a: Animal) => Dog;    // Takes Animal, returns Dog
type F2 = (d: Dog) => Animal;    // Takes Dog, returns Animal

// Which is the subtype?
// F1 <: F2  (F1 is a subtype of F2)

// Why?
// - Parameter: F1 accepts Animal (wider), F2 accepts Dog (narrower)
//   Contravariant: wider parameter makes it a subtype
//
// - Return: F1 returns Dog (narrower), F2 returns Animal (wider)  
//   Covariant: narrower return makes it a subtype

// F1 can be used anywhere F2 is expected
```

---

## Why Variance Matters

Variance rules exist to **prevent type safety violations**. Here's why:

```typescript
class Animal { name: string; }
class Dog extends Animal { bark(): void; }
class Cat extends Animal { meow(): void; }

// Scenario: Suppose List is covariant (like in some languages)
let dogs: List<Dog> = [new Dog(), new Dog()];
let animals: List<Animal> = dogs;  // Allowed if covariant

// Now the danger:
animals.add(new Cat());  // We just added a Cat...

// But remember, animals and dogs point to the SAME list!
dogs[2].bark();  // 💥 Runtime error! Cats can't bark!

// The type system said this was safe, but it wasn't.
// This is why mutable lists need to be INVARIANT.
```

**The type system uses variance rules to catch these errors at compile time rather than runtime.**

---

## Java's Approach to Variance

Java handles variance very differently from languages like TypeScript, Kotlin, or C#.

### Java Generics Are Invariant by Default

```java
class Animal { }
class Dog extends Animal { }

List<Animal> animals = new ArrayList<>();
List<Dog> dogs = new ArrayList<>();

// ❌ Does NOT compile in Java
animals = dogs;  // Error: incompatible types

// Java generics are INVARIANT - no relationship between List<Dog> and List<Animal>
```

**Why?** Java chose safety over convenience. Since `List` is mutable (read AND write), it must be invariant to prevent runtime errors.

### Java Uses Use-Site Variance (Wildcards)

Instead of declaring variance on the type constructor itself, Java lets you specify variance **at the point of use** with wildcards.

#### Covariance with `? extends` (Read-Only)

```java
List<Dog> dogs = List.of(new Dog(), new Dog());
List<? extends Animal> animals = dogs;  // ✅ OK!

Animal a = animals.get(0);  // ✅ Can read (covariant)
// animals.add(new Cat());  // ❌ Can't write! Compile error
// animals.add(new Dog());  // ❌ Can't even add Dog!
// animals.add(null);       // ✅ Only null is allowed

// Why can't we add? The compiler doesn't know the exact type
// animals could be List<Dog> or List<Cat> or List<Animal>
// So it can't guarantee type safety for writes
```

#### Contravariance with `? super` (Write-Only)

```java
List<Animal> animalList = new ArrayList<>();
List<? super Dog> dogContainer = animalList;  // ✅ OK!

dogContainer.add(new Dog());  // ✅ Can write (contravariant)
// Dog d = dogContainer.get(0); // ❌ Can't read as Dog! Only as Object
Object obj = dogContainer.get(0); // ✅ Can only get as Object
```

### PECS Principle

**P**roducer **E**xtends, **C**onsumer **S**uper

```java
// Producer: You READ from it (covariant)
public void processAnimals(List<? extends Animal> animals) {
    for (Animal a : animals) {  // ✅ Can read
        System.out.println(a);
    }
    // animals.add(new Dog());  // ❌ Can't write
}

processAnimals(new ArrayList<Dog>());  // ✅ Works!
processAnimals(new ArrayList<Cat>());  // ✅ Works!


// Consumer: You WRITE to it (contravariant)  
public void addDogs(List<? super Dog> container) {
    container.add(new Dog());  // ✅ Can write
    // Dog d = container.get(0); // ❌ Can't read as Dog
}

addDogs(new ArrayList<Animal>());  // ✅ Works!
addDogs(new ArrayList<Dog>());     // ✅ Works!
```

### Java Arrays: A Design Mistake

Java arrays ARE covariant (unlike generics), and this is widely considered a **design flaw**:

```java
Dog[] dogs = new Dog[]{new Dog(), new Dog()};
Animal[] animals = dogs;  // ✅ Compiles! Arrays are covariant

// This compiles but fails at RUNTIME:
animals[0] = new Cat();  // 💥 ArrayStoreException at runtime!

// The JVM has to check EVERY array write at runtime
// This is a performance cost AND it defeats the purpose of static typing

// This is why generics were made invariant by default - 
// to learn from this mistake and catch errors at compile time
```

This is a historical mistake from Java 1.0 that couldn't be fixed without breaking compatibility.

### Method Overriding Variance in Java

```java
class Animal { }
class Dog extends Animal { }

interface AnimalProcessor {
    Animal process(Animal a);
}

class DogProcessor implements AnimalProcessor {
    // ✅ Covariant return type (narrower return is OK)
    @Override
    public Dog process(Animal a) {  
        return new Dog();
    }
    
    // ❌ Can't make parameter contravariant in Java
    // @Override
    // public Animal process(Dog d) { }  // Won't compile!
    // Parameter types must be EXACTLY the same (invariant)
}
```

**Java only supports:**
- ✅ Covariant return types (since Java 5)
- ❌ Contravariant parameters (not supported - must be exact match)

---

## Comparison Across Languages

| Feature | TypeScript | Java | Kotlin/C# |
|---------|-----------|------|-----------|
| **Generic variance** | Structural, covariant by default | Invariant by default | Declaration-site variance (`out`/`in`) |
| **Variance specification** | Implicit | Use-site (`? extends`/`? super`) | Declaration-site + use-site |
| **Array variance** | Covariant | Covariant (mistake) | Invariant |
| **Function params** | Bivariant (compromise) | Invariant for overrides | Contravariant |
| **Function returns** | Covariant | Covariant | Covariant |

### Why Java Made These Choices

1. **Type erasure**: Java generics compile to raw types, making some variance rules harder to enforce
2. **Backward compatibility**: Arrays were already covariant in Java 1.0
3. **Safety over convenience**: Better to be restrictive and require wildcards than allow unsafe operations
4. **Use-site flexibility**: Wildcards give you more control at the call site

---

## Summary

### What IS Variance?

**Variance** = The rule that determines how subtyping relationships are transformed when you apply a type constructor

For any type constructor `F<T>`:
- **Covariant**: Preserves subtyping direction → `Dog <: Animal` implies `F<Dog> <: F<Animal>`
- **Contravariant**: Reverses subtyping direction → `Dog <: Animal` implies `F<Animal> <: F<Dog>`
- **Invariant**: Destroys subtyping relationship → No relationship between `F<Dog>` and `F<Animal>`

### For Functions `(A) => R`:
- Parameter position `A` is **contravariant** (wider is subtype)
- Return position `R` is **covariant** (narrower is subtype)

### Memory Trick: PECS

**P**roducer **E**xtends (Covariant), **C**onsumer **S**uper (Contravariant)

- **Producers** (output/read) → Covariant (use `extends` in Java, `out` in Kotlin/C#)
- **Consumers** (input/write) → Contravariant (use `super` in Java, `in` in Kotlin/C#)

### Key Takeaway for Java

In Java:
- **Generics are invariant** - you must use wildcards to get variance
- **`? extends T`** = covariance (read-only, producer)
- **`? super T`** = contravariance (write-only, consumer)
- **Arrays are covariant** = a design mistake, causes runtime errors

---

## Real-World Example (C#)

```csharp
// C# example showing all concepts
class Animal { }
class Dog : Animal { }
class Cat : Animal { }

// 1. Covariance (out keyword) - for producers
interface IProducer<out T> {
    T Produce(); // Only returns T, never consumes it
}

IProducer<Dog> dogProducer = ...;
IProducer<Animal> animalProducer = dogProducer; // ✅ Covariant

// 2. Contravariance (in keyword) - for consumers
interface IConsumer<in T> {
    void Consume(T item); // Only consumes T, never returns it
}

IConsumer<Animal> animalConsumer = ...;
IConsumer<Dog> dogConsumer = animalConsumer; // ✅ Contravariant

// 3. Invariance (no keyword) - for both
interface IRepository<T> {
    T Get();      // Returns T (wants covariance)
    void Add(T item); // Consumes T (wants contravariance)
}
// Must be invariant because of conflicting needs
```

---

*This document provides a comprehensive overview of variance in type systems. Understanding these concepts is crucial for writing type-safe code and understanding compiler errors related to type compatibility.*