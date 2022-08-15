# Resolving and Binding

1. Why is it safe to eagerly define the variable bound to a function’s name when
   other variables must wait until after they are initialized before they can be
   used?

   **Answer:**

   Because our grammar doesn't allow functions to be bound later (they're
   declared and initialized at the same time).

2. How do other languages you know handle local variables that refer to the same
   name in their initializer, like:

   ```
   var a = "outer";
   {
      var a = a;
   }
   ```

   Is it a runtime error? Compile error? Allowed? Do they treat global variables
   differently? Do you agree with their choices? Justify your answer.

3. Extend the resolver to report an error if a local variable is never used.

4. Our resolver calculates which environment the variable is found in, but it’s
   still looked up by name in that map. A more efficient environment representation
   would store local variables in an array and look them up by index.

   Extend the resolver to associate a unique index for each local variable declared
   in a scope. When resolving a variable access, look up both the scope the
   variable is in and its index and store that. In the interpreter, use that to
   quickly access a variable by its index instead of using a map.

   **Answer:**

   Out of convenience I used the expr var node to store the depth and that of
   course breaks the REPL. This approach could help.
