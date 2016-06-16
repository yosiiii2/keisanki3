syntax sugar remover  
====

Overview  
This program remove syntax sugars of small C source.  
The input file is accepted as command line argument.  
And return AST of the source.  

## Requirement  
library :  
parsec 3.1.9  

build tool :  
stack 1.0.4.3  

## How to Build
in the directory `sugar/` you run the command:  
`$stack build`  
and then you make some small C code, and  
`$stack exec sugar-exe hoge.sc`  
then you can see the AST of the code.

