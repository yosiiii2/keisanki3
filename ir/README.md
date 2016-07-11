small C intermediate representation
===

Overview  
This program make an Intermediate representation of small C code.  
The input file is accepted as command line argument.  

## Requirement
library :  
parsec 3.1.9  

build tool :  
stack 1.0.4.3  

## How to Build
in the directory `/ir` you run the command:  
`$stack build`  
and then you make some small C code, and  
`$stack exec ir-exe hoge.sc`  
then you can see the AST of the code.

