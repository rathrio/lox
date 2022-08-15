# Statements and State

1. The REPL no longer supports entering a single expression and automatically
   printing its result value. That’s a drag. Add support to the REPL to let users
   type in both statements and expressions. If they enter a statement, execute it.
   If they enter an expression, evaluate it and display the result value.

2. Maybe you want Lox to be a little more explicit about variable
   initialization. Instead of implicitly initializing variables to nil, make it a
   runtime error to access a variable that has not been initialized or assigned to,
   as in:

   ```
   // No initializers.
   var a;
   var b;

   a = "assigned";
   print a; // OK, was assigned first.

   print b; // Error!
   ```

3. What does the following program do?

   ```
   var a = 1;
   {
        var a = a + 2;
        print a;
   }
   ```

   What did you expect it to do? Is it what you think it should do? What does
   analogous code in other languages you are familiar with do? What do you think
   users will expect this to do?

   **Answer:**

   I expected 3 and it indeed prints 3. Some language may evaluate the LHS of an
   assignment expression first (with `nil`). In that case this would throw a
   runtime error.

   I usually avoid shadowing as it's still confusing and I think most people
   generally avoid this.
