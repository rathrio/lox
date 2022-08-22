# Classes

1. We have methods on instances, but there is no way to define “static” methods
   that can be called directly on the class object itself. Add support for them.
   Use a class keyword preceding the method to indicate a static method that hangs
   off the class object.

   ```
   class Math {
     class square(n) {
       return n * n;
     }
   }

   print Math.square(3); // Prints "9".
   ```

   You can solve this however you like, but the “metaclasses” used by Smalltalk and
   Ruby are a particularly elegant approach. Hint: Make LoxClass extend LoxInstance
   and go from there.

   **Answer:**

   Although I'm very fond of Ruby's approach, I think it's overkill
   for what we're allowing in Lox and complicating the current design in Rust.
   E.g. it's not possible in Lox to add methods to single instances.

   Adding a simple class methods map to the `Class` struct should do.

   See `lox::interpreter::tests::test_class_methods`.
