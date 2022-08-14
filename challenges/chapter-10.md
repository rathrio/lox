# Functions

1. Our interpreter carefully checks that the number of arguments passed to a
   function matches the number of parameters it expects. Since this check is done
   at runtime on every call, it has a performance cost. Smalltalk implementations
   don’t have that problem. Why not?

   Smalltalk doesn't have positional arguments, but rather keyword args that are
   part of the signature of the method itself.

2. Lox’s function declaration syntax performs two independent operations. It
   creates a function and also binds it to a name. This improves usability for the
   common case where you do want to associate a name with the function. But in
   functional-styled code, you often want to create a function to immediately pass
   it to some other function or return it. In that case, it doesn’t need a name.

   Languages that encourage a functional style usually support anonymous functions
   or lambdas—an expression syntax that creates a function without binding it to a
   name. Add anonymous function syntax to Lox so that this works:

   **Answer:** See `lox::interpreter::tests::test_anon_functions`.

   ```
   fun thrice(fn) {
      for (var i = 1; i <= 3; i = i + 1) {
         fn(i);
      }
   }

   thrice(fun (a) {
      print a;
   });
   ```

   How do you handle the tricky case of an anonymous function expression
   occurring in an expression statement:

   ```
   fun () {};
   ```

   **Answer**:

   One approach: Allow it and emit warning in another phase.

   I'm currently dissallowing this in the grammar. The parser provides a hint
   that anonymous functions are not allowed in this context.

3. Is this program valid?

   ```
   fun scope(a) {
      var a = "local";
   }
   ```

   In other words, are a function’s parameters in the same scope as its local
   variables, or in an outer scope? What does Lox do? What about other languages
   you are familiar with? What do you think a language should do?

   **Answer:**

   Some allow shadowing, some don't. I'm actually a little conflicted on this.
   Especially in dynamically typed langauges I oftentimes use some assetions
   and/or casting and redefine the parameter.

   Rust allows you to redefine, but also warn you about unused parameters by
   default. I think this is a sweet spot.
