# Representing Code

1. Earlier, I said that the `|`, `*`, and `+` forms we added to our grammar
   metasyntax were just syntactic sugar. Take this grammar:

   ```
   expr → expr ( "(" ( expr ( "," expr )* )? ")" | "." IDENTIFIER )+
     | IDENTIFIER
     | NUMBER
   ```

   Produce a grammar that matches the same language but does not use any of that
   notational sugar.

   ```
   expr -> expr invoke
   expr -> identifier
   expr -> number

   invoke -> "(" expr comma_expr ")"
   invoke -> "." identifier
   invoke -> access

   comma_expr -> "," expr
   comma_expr -> <epsilon>
   ```

   foo(23, 42)
   23.id

   > Bonus: What kind of expression does this bit of grammar encode?

   Property access or method call.

2. The Visitor pattern lets you emulate the functional style in an
   object-oriented language. Devise a complementary pattern for a functional
   language. It should let you bundle all of the operations on one type together
   and let you define new types easily.

   (SML or Haskell would be ideal for this exercise, but Scheme or another Lisp
   works as well.)

   Already using Rust with an ML-like type system.

3. In reverse Polish notation (RPN), the operands to an arithmetic operator are
   both placed before the operator, so 1 + 2 becomes 1 2 +. Evaluation proceeds
   from left to right. Numbers are pushed onto an implicit stack. An arithmetic
   operator pops the top two numbers, performs the operation, and pushes the
   result. Thus, this:

   ```
   (1 + 2) * (4 - 3)
   ```

   in RPN becomes:

   ```
   1 2 + 4 3 - *
   ```

   Define a visitor class for our syntax tree classes that takes an expression,
   converts it to RPN, and returns the resulting string.

   See `lox::ast::rpn(e: &Expr)`.
