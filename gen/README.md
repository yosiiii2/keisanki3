MIPS generator
===

Overview  
This program make a MIPS code.  
The input file is accepted as command line argument.  

## Requirement
library :  
parsec 3.1.9  

build tool :  
stack 1.0.4.3  

## How to Build
in the directory `/gen` you run the command:  
`$stack build`
and then you make some small C code, and  
`$stack exec gen-exe hoge.sc`  
then you can see the MIPS code of the small C code.

