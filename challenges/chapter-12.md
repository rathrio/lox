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

2. Most modern languages support “getters” and “setters”—members on a class that
   look like field reads and writes but that actually execute user-defined code.
   Extend Lox to support getter methods. These are declared without a parameter
   list. The body of the getter is executed when a property with that name is
   accessed.

   ```
   class Circle {
     init(radius) {
       this.radius = radius;
     }

     area {
       return 3.141592653 * this.radius * this.radius;
     }
   }

   var circle = Circle(4);
   print circle.area; // Prints roughly "50.2655".
   ```

   **Answer:**

   Did not implement this, but here's the approaches I see:

   - During parsing, when parens are missing, parse them as regular function,
     but store them in a dedicated getters map in the class. When no field can be
     found on an instance when interpreting a get-expression, lookup the getter
     function and immediateliy evaluate it if present.

   - If we had built a full symbol table, one could also desugar this into a
     regular function call.

3. Python and JavaScript allow you to freely access an object’s fields from
   outside of its own methods. Ruby and Smalltalk encapsulate instance state. Only
   methods on the class can access the raw fields, and it is up to the class to
   decide which state is exposed. Most statically typed languages offer modifiers
   like private and public to control which parts of a class are externally
   accessible on a per-member basis.

   What are the trade-offs between these approaches and why might a language prefer
   one or the other?

   **Answer:**

   Pros of freely accessing fields:

   - No boilerplate / more ergonomic when it comes to writing data classes

   - A simpler grammar as there's no special access management rules

   Cons (and props of more explicit access management):

   - Harder to design public APIs. One may want to hide implementation detail
     state that is not meant to be directly accessed and may be prone to more
     change that the public API. I think the python community solves this by
     prefix underscores or something.

   - Harder to statically understand what is local state that is not accessed
     elsewhere in a program.

   - Somestimes one always wants certain behaviour to happen when state is
     changed, so one would introduce a public write API that hides this. If the
     field is public, users may directly write the field without doing the special
     procedure that needs to run as well. Reactive programming patterns could
     solve this in an arguably more elegent way.