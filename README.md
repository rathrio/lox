# Lox

## Todo

- [x] Simple benchmarks
  - [x] How much time is spent in parser? (neglectable)
  - [ ] AST interpreter perf: Make method lookup faster?

- [ ] Fix REPL (again...)


## Running tests

```sh
cargo test
```

```sh
cargo jlox-suite

# whereas ~/scripts/cargo-jlox-suite:
#!/bin/bash

cargo install --path .
cd ~/git/craftinginterpreters/
dart tool/bin/test.dart jlox --interpreter lox
cd -
```


## Resources

The book:

- https://craftinginterpreters.com

Lox Grammar:

- https://craftinginterpreters.com/appendix-i.html

What's in the Box??

- https://users.rust-lang.org/t/is-there-a-better-way-to-represent-an-abstract-syntax-tree/9549/2

Pratt Parsing:

- https://www.youtube.com/watch?v=qyZQPJYvsGw
- https://www.youtube.com/watch?v=MnctEW1oL-E&t=4510s
- https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- https://www.oilshell.org/blog/2017/03/31.html

Reference Test Suite:

- https://github.com/munificent/craftinginterpreters#testing-your-implementation
