# HaSQL

Students
5676304
5687101
5692024
5601118
5620724
5666503


## Running

Use [Stack](https://docs.haskellstack.org/en/stable/README/) to run the
application:

``` shell
stack run
```


All source files are contained in the 'scr' folder. This contains the following files:

Algebra.hs
This contains the algebraic definition used for the fold used in the static type checking 
and code generation.

Static.hs
Contains the code for the Static Semantics

Dynamic.hs
Contains the code representing the Dynamic Semantics which generate code.

Lib.hs
Contains examples to be run

Syntax.hs
Contains the Abstract syntax of our language in Haskell Data types



How to run an example:
- Compile Lib.hs
- Then run one of the example functions (example, exampleAdd, exampleNorm, exampleDecouple, exampleSplit) as follows
- execute "stack run"
- then run an example by running: "compile [example_name]"
