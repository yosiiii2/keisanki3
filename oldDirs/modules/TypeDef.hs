module TypeDef where

import Text.Parsec.Pos
                         
data External = FuncProto SourcePos Type FunctionDec 
              | FunctionDef SourcePos Type FunctionDec Stmt 
              | Declaration SourcePos Type [Decralator] 
                deriving Show

data Decralator = PointDec SourcePos DDec
                | Dec SourcePos DDec  
                  deriving Show
                  
data DDec = DirectDec SourcePos String
          | ArrayDec SourcePos String Integer
            deriving Show
            
data FunctionDec = FuncDec SourcePos String [ParameterDeclaration] 
                 | PointFuncDec SourcePos String [ParameterDeclaration] 
                   deriving Show
                   
data ParameterDeclaration = ParaDec SourcePos Type String 
                          | PointParaDec SourcePos Type String 
                            deriving Show
                            
data Stmt = Statement SourcePos Exp 
          | SemiOnly SourcePos
          | CompStmt SourcePos [External] [Stmt] 
          | If SourcePos Exp Stmt 
          | IfElse SourcePos Exp Stmt Stmt 
          | While SourcePos Exp Stmt
          | For SourcePos Exp Exp Exp Stmt
          | Return SourcePos Exp
            deriving Show

data Exp = Assign SourcePos Exp Exp 
         | Or SourcePos Exp Exp 
         | And SourcePos Exp Exp 
         | Equal SourcePos Exp Exp 
         | NotEqual SourcePos Exp Exp 
         | Small SourcePos Exp Exp 
         | Large SourcePos Exp Exp 
         | SmallEq SourcePos Exp Exp 
         | LargeEq SourcePos Exp Exp 
         | Add SourcePos Exp Exp 
         | Sub SourcePos Exp Exp 
         | Mul SourcePos Exp Exp 
         | Div SourcePos Exp Exp 
         | Neg SourcePos Exp 
         | Address SourcePos Exp 
         | Pointer SourcePos Exp 
         | Array SourcePos Exp Exp
         | Func SourcePos Exp [Exp]
         | Id SourcePos String
         | Const SourcePos Integer
         | Paren SourcePos Exp 
         | ManyExp SourcePos [Exp] 
           deriving Show
                    
data Type = Int SourcePos
          | Void SourcePos
            deriving Show
