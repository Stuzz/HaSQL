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

``` shell
stack run
```

## Project structure

All source files are contained in the `src` directory. This contains the
following files:

| File         | Description                                                        |
| ------------ | ------------------------------------------------------------------ |
| `Algebra.hs` | Fold structure                                                     |
| `Static.hs`  | Contains the code for the Static Semantics                         |
| `Dynamic.hs` | Contains the code representing the Dynamic Semantics               |
| `Lib.hs`     | Contains examples to be run                                        |
| `Syntax.hs`  | Contains the Abstract syntax of our language in Haskell Data types |

How to run an example:

1. Compile Lib.hs
2. Then run one of the example functions (`example`, `exampleAdd`,
   `exampleNorm`, `exampleDecouple`, `exampleSplit`) as follows
3. Execute `stack run`
4. Then run an example by running: `compile [example_name]`
