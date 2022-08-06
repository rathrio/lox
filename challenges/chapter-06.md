1. In C, a block is a statement form that allows you to pack a series of
   statements where a single one is expected. The comma operator is an analogous
   syntax for expressions. A comma-separated series of expressions can be given
   where a single expression is expected (except inside a function call’s argument
   list). At runtime, the comma operator evaluates the left operand and discards
   the result. Then it evaluates and returns the right operand.

   Add support for comma expressions. Give them the same precedence and
   associativity as in C. Write the grammar, and then implement the necessary
   parsing code.

   See `lox::parser::tests::test_comma_expr`.

2. Likewise, add support for the C-style conditional or “ternary” operator ?:.
   What precedence level is allowed between the ? and :? Is the whole operator
   left-associative or right-associative?

   See `lox::parser::tests::test_ternary_expr`.

3. Add error productions to handle each binary operator appearing without a
   left-hand operand. In other words, detect a binary operator appearing at the
   beginning of an expression. Report that as an error, but also parse and discard
   a right-hand operand with the appropriate precedence.
