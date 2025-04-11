# Interpreter for a flattened arithmetic language

OCaml port of Adrian Sampson's [Rust interpreter for a flattened calculator language](https://github.com/sampsyo/flatcalc), 
detailed in [his blogpost](https://www.cs.cornell.edu/~asampson/blog/flattening.html).

- [`first.ml`](./lib/first.ml): naive interpreter
- [`second.ml`](./lib/first.ml): interpreter for flattened AST

This repo compiles with `dune build`. Run `opam install --yes . --deps-only` to install all OCaml dependencies. 

