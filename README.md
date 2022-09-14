# Lox

The [Lox programming language](https://craftinginterpreters.com/the-lox-language.html)
implemented in Rust.

Currently contains a tree walking interpreter as described in part II of the
book. It passes the `jlox` reference test suite (except for some error message
formats / line reporting differences). The bytecode VM is WIP.

Notable differences to `jlox` (besides not being written in Java):

- Uses Pratt's algorithm to parse expressions (I've only now learned that part
  III actually introduced Pratt's approach as well...). The resources sections
  contains a bunch of references to tutorials I found very helpful.

- Does not have a second resolver pass. Scopes are resolved during parsing. I no
  longer like this approach as I'd like to reuse the same AST for emitting
  bytecode, and it currently contains a bunch of things specific to the tree
  walking interpreter.

- Is surprisingly slower in many of the benchmarks. According to the
  flamegraphs, there's a lot of potential for improvement in how values are
  stored in environments. Hash maps are surprisingly slow and one should
  definitely consider switching to vectors, but I'd rather move on to the
  bytecode VM.


## Todos

- [ ] Bytecode VM
- [ ] Fix REPL (again...)

## Running tests

```sh
cargo test
```

```sh
cargo jlox-suite

# whereas ~/scripts/cargo-jlox-suite: (or somewhere else in your path)
#!/bin/bash

cargo install --path .
cd ~/git/craftinginterpreters/
dart tool/bin/test.dart jlox --interpreter lox
cd -
```

## Generating a flamegraph

```sh
cargo install flamegraph
cargo install --path .
cd ~/git/craftinginterpreters/
sudo flamegraph -- lox test/benchmark/instantiation.lox
open flamegraph.svg
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
