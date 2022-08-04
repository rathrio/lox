# Lox

## Chapter 4 challenges

1. The lexical grammars of Python and Haskell are not regular. What does that
   mean, and why aren’t they?

2. Aside from separating tokensl — distinguishing `print foo` from `printfoo` —
   spaces aren’t used for much in most languages. However, in a couple of dark
   corners, a space does affect how code is parsed in CoffeeScript, Ruby, and the C
   preprocessor. Where and what effect does it have in each of those languages?

3. Our scanner here, like most, discards comments and whitespace since those
   aren’t needed by the parser. Why might you want to write a scanner that does not
   discard those? What would it be useful for?

4. Add support to Lox’s scanner for C-style `/* ... */` block comments. Make
   sure to handle newlines in them. Consider allowing them to nest. Is adding sup-
   port for nesting more work than you expected? Why?