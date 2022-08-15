# Control Flow

1. A few chapters from now, when Lox supports first-class functions and dynamic
   dispatch, we technically won’t need branching statements built into the
   language. Show how conditional execution can be implemented in terms of those.
   Name a language that uses this technique for its control flow.

   **Answer:**

   Smalltalk, e.g. `someValue ifTrue: [ ... ] ifFalse: [ ... ]`

2. Likewise, looping can be implemented using those same tools, provided our
   interpreter supports an important optimization. What is it, and why is it
   necessary? Name a language that uses this technique for iteration.

   **Answer:**

   Recursion. You'd want tail call optimization to avoid stack overflows.
   Haskell and Clojure are two that come to mind.

3. Unlike Lox, most other C-style languages also support break and continue
   statements inside loops. Add support for break statements.

   The syntax is a break keyword followed by a semicolon. It should be a syntax
   error to have a break statement appear outside of any enclosing loop. At
   runtime, a break statement causes execution to jump to the end of the nearest
   enclosing loop and proceeds from there. Note that the break may be nested inside
   other blocks and if statements that also need to be exited.

   **Answer:**

   Ok, I'm really proud of this one. In the parser I use a flag that is
   propagated down in order to denote whether the current context is breakable
   or not. In the interpreter I return a special enum with two variants. One
   that means that a loop can continue, the other variant will break it.
