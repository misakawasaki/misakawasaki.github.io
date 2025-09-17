### Introduction

`Balancing parentheses` is a classic problem often seen in coding exercises. The task is to determine whether a given string of parentheses is balanced. The problem can extend to other types of brackets, such as curly braces {} and square brackets [].

In this blog post, we'll explore a solution to the balancing-parentheses problem—extended to cover three kinds of brackets, "()", "[]", and "{}"—that is different and unusual. To be more precise, I will call this problem `Balancing Brackets`.

In this blog post, I will use `Java` to implement the solution.

Before we solve the problem, we need to define exactly what *balanced* brackets mean.  
A string of brackets is balanced if every opening symbol has a matching closing symbol and the pairs are properly nested.  
This definition gives us four criteria:

1. **Equal counts**: for every bracket type, the number of opening tokens equals the number of closing tokens.
2. **Order**: each opening bracket must appear earlier than its corresponding closing bracket.
3. **Type matching**: a closing bracket must pair with the most recent unmatched opening bracket of the same type.
4. **Nesting**: brackets may contain other brackets, but crossing types (e.g., `([)]`) is illegal.

### The Standard Way
Think of the string as a nesting chain: every time you hit an opening bracket you start a new, inner layer; every time you hit a closing bracket you must finish the most recently started layer.  
That “most-recent-first” obligation is exactly what a stack gives you:

- See an opener → push it on the stack.
- See a closer → the top of the stack must be the matching opener; pop and verify.

If at any point the top doesn't match, or you pop on an empty stack, the four rules are violated immediately.  
When the scan ends, an empty stack means every opened layer was properly closed in the reverse order it was opened—i.e., all four criteria are satisfied.

The idea translates to Java in a dozen lines.  
We scan the string left-to-right, maintain a `Deque<Character>` as our stack, and use a small helper map that maps each closing bracket to its expected opener.

- **Opener** (`(`, `[`, `{`) → push onto the stack.
- **Closer** (`)`, `]`, `}`) → pop once; if the popped symbol does not match the expected opener, return `false` immediately.

After the loop, the string is balanced **iff** the stack is empty.

The complete code is shown below.
```Java
public class BracketValidator {
    private static final Map<Character, Bracket> CHAR_TO_BRACKET = new HashMap<>();

    static {
        CHAR_TO_BRACKET.put('(', Bracket.LPAREN);
        CHAR_TO_BRACKET.put(')', Bracket.RPAREN);
        CHAR_TO_BRACKET.put('[', Bracket.LBRACKET);
        CHAR_TO_BRACKET.put(']', Bracket.RBRACKET);
        CHAR_TO_BRACKET.put('{', Bracket.LBRACE);
        CHAR_TO_BRACKET.put('}', Bracket.RBRACE);
    }

    public static boolean isValid(String s) {
        Deque<Character> stack = new ArrayDeque<>();
        for (char c : s.toCharArray()) {
            Bracket bracket = CHAR_TO_BRACKET.getOrDefault(c, Bracket.INVALID);
            if (!bracket.accept(stack)) {
                return false;
            }
        }
        return stack.isEmpty();
    }

    private enum Bracket {
        LPAREN {
            @Override
            public boolean accept(Deque<Character> stack) {
                stack.push('(');
                return true;
            }
        },
        RPAREN {
            @Override
            public boolean accept(Deque<Character> stack) {
                return !stack.isEmpty() && stack.pop() == '(';
            }
        },
        LBRACKET {
            @Override
            public boolean accept(Deque<Character> stack) {
                stack.push('[');
                return true;
            }
        },
        RBRACKET {
            @Override
            public boolean accept(Deque<Character> stack) {
                return !stack.isEmpty() && stack.pop() == '[';
            }
        },
        LBRACE {
            @Override
            public boolean accept(Deque<Character> stack) {
                stack.push('{');
                return true;
            }
        },
        RBRACE {
            @Override
            public boolean accept(Deque<Character> stack) {
                return !stack.isEmpty() && stack.pop() == '{';
            }
        },
        INVALID {
            @Override
            public boolean accept(Deque<Character> stack) {
                return false;
            }
        };

        public abstract boolean accept(Deque<Character> stack);
    }
}
```
Sure, the one-liner stack version fits in a tweet—but this **is** Java, so we celebrate the rite of **over-engineering**:

- Enum-powered parser? Check.
- Abstract method per bracket? Double check.
- HashMap dispatch for six characters? Why not.

Is it heavier than `if (c == '(') stack.push(c);`?  
Absolutely.  
But hey, we’re in Java country—if the class count isn’t ≥ 3, did we even *solve* the problem?

Live with it, or `import sarcasm;`.

### The natural Way
In this solution, we do not solve the problem by relying on its incidental features or properties; doing so makes problem and solution heteromorphic. If we misread those features, we simply write the wrong program.

Instead, we derive the solution directly from the problem's description—its definition. This keeps problem and solution isomorphic. Once the definition is stated clearly, the correct program becomes obvious.

For balanced brackets, the formal definition is elegantly simple:**

1. The empty string is balanced.
2. A balanced string is any finite concatenation of *groups*, where each group has one of the forms:  
   `"("` + *balanced* + `")"`  
   `"["` + *balanced* + `"]"`  
   `"{"` + *balanced* + `"}"`

This definition is not only mathematically natural — it is *executable*. The structure of the program mirrors the grammar itself: the source code becomes a direct, verbatim encoding of the formal specification.

What may appear as “complexity” in the parser combinators is, in fact, the *price of fidelity* — the cost of refusing to distort the grammar for implementation convenience. Once the reader recognizes the one-to-one correspondence between production rules and combinators, the solution becomes **self-evident**, **mechanically derivable**, and **highly resistant to error**.
```ebnf
balanced ::= group*
group    ::= '(' balanced ')' | '[' balanced ']' | '{' balanced '}'
```
To implement this definition *literally*, we need only four primitive combinators — each directly reflecting an EBNF construct:

| EBNF Construct       | Parser Combinator         | Role & Example                                  |
|----------------------|---------------------------|------------------------------------------------|
| Terminal `'x'`       | `literal("x")`            | Matches exact string: `literal("(")`           |
| Concatenation `A B`  | `sequence(A, B, ...)`     | Matches parsers in order: `sequence(lit("("), balanced, lit(")"))` |
| Alternation `A \| B` | `choice(A, B, ...)`       | Matches first (or longest) success: `choice(seq1, seq2, seq3)` |
| Repetition `A*`      | `many(A)`                 | Matches zero or more: `many(group)`            |

> Each combinator is a function that takes parsers as input and returns a new parser — composing structure exactly as the grammar prescribes.  
> There is no “glue code”, no “driver loop”, no “AST builder” — just the grammar, made runnable.

The resulting parser is not an “implementation” of the grammar — **it *is* the grammar, made executable**.

- ✅ No translation layer
- ✅ No intermediate representation
- ✅ No distortion for performance or convenience
- ✅ The code *is* the specification

This is the essence of **parser combinators**: turning grammar rules into first-class functions, preserving structure, meaning, and intent — so that correctness becomes a natural consequence of form.

> 💡 *"When the grammar and the code are isomorphic, bugs have nowhere to hide."*

The only remaining task is to implement the **parser combinators** that **directly mirror** this definition:  
`many`, `choice`, `sequence`, and `literal`.

```Java
    @FunctionalInterface
    interface Parser {
        Either<String, Boolean> parse(String input);
    }

    private static Parser literal(String target) {
        return input -> input.startsWith(target) ? Either.left(target) : Either.right(false);
    }

    private static Parser sequence(Parser... parsers) {
        return input -> sequenceRec(parsers, 0, input, "");
    }

    private static Either<String, Boolean> sequenceRec(Parser[] parsers,
                                                   int idx,
                                                   String rest,
                                                   String acc) {
        if (idx == parsers.length) {
            return Either.left(acc);
        }
 
        Either<String, Boolean> result = parsers[idx].parse(rest);
        if (result.isRight()) {         
            return result;
        }
    
        String matched = result.getLeft();
        return sequenceRec(parsers, idx + 1,
                            rest.substring(matched.length()),
                        acc + matched);
    }


    private static Parser choice(Parser... parsers) {
        return input -> findBest(parsers, 0, input, 0, Either.right(false));
    }
    
    private static Either<String, Boolean> findBest(Parser[] parsers,
                                                int idx,
                                                String rest,
                                                int bestLen,
                                                Either<String, Boolean> best) {
        if (idx == parsers.length) {          
            return best;
        }
        
        Either<String, Boolean> result = parsers[idx].parse(rest);
        if (result.isLeft()) {
            String matched = result.getLeft();
            int len = matched.length();
            if (len > bestLen) {             
                best = result;
                bestLen = len;
            }
        }
        return findBest(parsers, idx + 1, rest, bestLen, best);
    }
    
    private static Parser many(Parser parser) {
        return input -> manyRec(parser, input, "");
    }
    
    private static Either<String, Boolean> manyRec(Parser parser,
                                               String rest,
                                               String acc) {
        Either<String, Boolean> result = parser.parse(rest);
        if (result.isLeft()) {
            String matched = result.getLeft();
            if (!matched.isEmpty()) {                 
                return manyRec(parser,
                    rest.substring(matched.length()),
                    acc + matched);
            }
        }
        return Either.left(acc);
    }
```
With `many`, `choice`, `sequence`, and `literal` in hand, we can now *quote* the grammar verbatim. The `balanced` parser is nothing more than **a direct, left-to-right translation** of Rule 2 from the formal definition:
```Java
    private static Parser balanced() {
        return input ->
                    many(
                        choice(
                                sequence(literal("("), balanced(), literal(")")),
                                sequence(literal("["), balanced(), literal("]")),
                                sequence(literal("{"), balanced(), literal("}"))
                        )
                    ).parse(input);
    }
```
Finally, the complete implementation:
```Java
import io.vavr.control.Either;

public class BracketValidator {

    /* ==== core combinators ==== */

    private static Parser literal(String target) {
        return input -> input.startsWith(target) ? Either.left(target) : Either.right(false);
    }

    private static Parser sequence(Parser... parsers) {
        return input -> sequenceRec(parsers, 0, input, "");
    }

    private static Either<String, Boolean> sequenceRec(Parser[] ps, int idx, String rest, String acc) {
        if (idx == ps.length) return Either.left(acc);
        Either<String, Boolean> res = ps[idx].parse(rest);
        if (res.isRight()) return res;
        String m = res.getLeft();
        return sequenceRec(ps, idx + 1, rest.substring(m.length()), acc + m);
    }

    private static Parser choice(Parser... parsers) {
        return input -> findBest(parsers, 0, input, 0, Either.right(false));
    }

    private static Either<String, Boolean> findBest(Parser[] ps, int idx, String rest, int bestLen, Either<String, Boolean> best) {
        if (idx == ps.length) return best;
        Either<String, Boolean> res = ps[idx].parse(rest);
        if (res.isLeft()) {
            int len = res.getLeft().length();
            if (len > bestLen) {
                best = res;
                bestLen = len;
            }
        }
        return findBest(ps, idx + 1, rest, bestLen, best);
    }

    private static Parser many(Parser p) {
        return input -> manyRec(p, input, "");
    }

    private static Either<String, Boolean> manyRec(Parser p, String rest, String acc) {
        Either<String, Boolean> res = p.parse(rest);
        if (res.isLeft()) {
            String m = res.getLeft();
            if (!m.isEmpty()) return manyRec(p, rest.substring(m.length()), acc + m);
        }
        return Either.left(acc);
    }

    /* ==== grammar ==== */

    private static Parser balanced() {
        return input ->
                many(
                        choice(
                                sequence(literal("("), balanced(), literal(")")),
                                sequence(literal("["), balanced(), literal("]")),
                                sequence(literal("{"), balanced(), literal("}"))
                        )
                ).parse(input);
    }

    public static boolean isValid(String s) {
        Either<String, Boolean> result = balanced().parse(s);
        return result.isLeft() && result.getLeft().equals(s);
    }

    /* ==== infrastructure ==== */

    @FunctionalInterface
    public interface Parser {
        Either<String, Boolean> parse(String input);
    }
}
```

