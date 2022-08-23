# Inheritance

1. Lox supports only single inheritance—a class may have a single superclass and
   that’s the only way to reuse methods across classes. Other languages have
   explored a variety of ways to more freely reuse and share capabilities across
   classes: mixins, traits, multiple inheritance, virtual inheritance, extension
   methods, etc.

   If you were to add some feature along these lines to Lox, which would you pick
   and why? If you’re feeling courageous (and you should be at this point), go
   ahead and add it.

   **Answer:**

   I'd probably not include any. In my experience, programmers will always
   misuse inheritance as the most convenient way to share code which results in
   tightly coupled, hard to read hierarchies.

   To share behaviour only, something like Ruby's modules are fine.
