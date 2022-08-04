1. The lexical grammars of Python and Haskell are not regular. What does that
   mean, and why aren’t they?

   Non regular would mean at least context free, i.e., we'd need additional
   state to keep track of nesting. I can imagine the indentation sensitivity to
   force the scanner to keep track of the current nesting level.

2. Aside from separating tokens — distinguishing `print foo` from `printfoo` —
   spaces aren’t used for much in most languages. However, in a couple of dark
   corners, a space does affect how code is parsed in CoffeeScript, Ruby, and the C
   preprocessor. Where and what effect does it have in each of those languages?

3. Our scanner here, like most, discards comments and whitespace since those
   aren’t needed by the parser. Why might you want to write a scanner that does not
   discard those? What would it be useful for?

   - A pretty printer that preserves comments and developer line breaks / misc
     whitespace.

   - Type checkers that rely on documentation info for types, e.g. Ruby's
     solargraph with yard docs.

   - Any kind of tooling that wants to figure out where exactly constructs
     (their spans) are located.

4. Add support to Lox’s scanner for C-style `/* ... */` block comments. Make
   sure to handle newlines in them. Consider allowing them to nest. Is adding sup-
   port for nesting more work than you expected? Why?
   
   As soon as we allow nesting we need additional state to keep track of the
   level, so the langauge is no longer regular and requires another parsing
   approach, e.g., recursive descent.