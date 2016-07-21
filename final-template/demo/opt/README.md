MIPS generator
===

Overview
This program do optimisation for IR.
The input file is accepted as command line argument.

## Requirement
library :
parsec 3.1.9

build tool :
stack 1.0.4.3

## How to Build
in the directory `/opt` you run the command:
`$stack install`
and then you make some small C code, and
`$stack exec gen-exe hoge.sc`
then you can see the MIPS code of the small C code.
