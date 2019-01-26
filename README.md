# HaSQL

| Students |
| -------- |
| 5676304  |
| 5687101  |
| 5692024  |
| 5601118  |
| 5620724  |
| 5666503  |

## Running

Use [Stack](https://docs.haskellstack.org/en/stable/README/) to run the
application:

```shell
stack run
```

The examples mentioned in the report are located in the `examples/` directory.
They can be run by passing them through STDIN:

```shell
stack run < examples/decouple.hasql
```

Additional examples are available in AST form, located in `src/lib.hs`.

## Project structure

All source files are contained in the `src` directory. This contains the
following files:

| File         | Description                                                              |
| ------------ | ------------------------------------------------------------------------ |
| `Algebra.hs` | This contains the algebraic definition used in the fold over the AST     |
| `Static.hs`  | Contains the code for the Static Semantics                               |
| `Dynamic.hs` | Contains the code representing the Dynamic Semantics for code generation |
| `Lib.hs`     | Contains examples to be run                                              |
| `Syntax.hs`  | Contains the Abstract syntax of our language in Haskell Data types       |
