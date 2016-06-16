module TypeDef where

import Text.Parsec.Pos
import Data.List

type AST = [External]
type PointOr = Bool
type ArrayNum = Integer


data External = FuncProto SourcePos Type PointOr String [Declarator]
              | FunctionDef SourcePos Type PointOr String [Declarator] Stmt 
              | Declaration SourcePos [Declarator]
                deriving Show
                
-- instance Show External where
--     show (FuncProto _ a b c d) = ("FuncProto " ++ (show a) ++ " " ++(show b) ++ " " ++ (show c) ++ " " ++ ((intercalate " ") (map show d)))
--     show (FunctionDef _ a b c d e) = ("FunctionDef " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (intercalate " " (map show d)) ++ " " ++ show e)
--     show (Declaration _ a) = ("Declaraion " ++ " " ++ (show a))
                             

data Declarator = Declarator SourcePos Type String PointOr ArrayNum
                deriving Show
-- instance Show Declarator where
--     show (Declarator _ a b c d) = ("Declarator " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d))


data Stmt = Statement SourcePos Exp 
          | SemiOnly SourcePos
          | CompStmt SourcePos [External] [Stmt] 
          | If SourcePos Exp Stmt 
          | IfElse SourcePos Exp Stmt Stmt 
          | While SourcePos Exp Stmt
          | For SourcePos Exp Exp Exp Stmt
          | Return SourcePos Exp
          | VReturn SourcePos
            deriving Show
-- instance Show Stmt where
--     show (Statement _ a) = "Statement " ++ (show a)
--     show (SemiOnly _) = "SemiOnly "
--     show (CompStmt _ a b) = ("CompStmt " ++ ((intercalate " ") $ map show a) ++ " " ++ ((intercalate " ") $ map show b))
--     show (If _ a b) = "If " ++ (show a) ++ " " ++ (show b)
--     show (IfElse _ a b c) = "IfElse " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c)
--     show (While _ a b) = ("While " ++ (show a) ++ " " ++ (show b))
--     show (For _ a b c d) = "For " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d)
--     show (Return _ a) = "Return " ++ (show a)
--     show (VReturn _) = "VReturn "


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
         | Func SourcePos String [Exp]
         | Id SourcePos String
         | Const SourcePos Integer
         | Paren SourcePos Exp 
         | ManyExp SourcePos [Exp] 
           deriving Show
           
-- instance Show Exp where
--     show (Or _ a b) = "Or " ++ show a ++ " " ++ show b
--     show (And _ a b) = "And " ++ show a ++ " " ++ show b  
--     show (Equal _ a b) = "Equal " ++  show a ++ " " ++ show b
--     show (NotEqual _ a b) = "NotEqual " ++ show a ++ " " ++ show b
--     show (Small _ a b) = "Small " ++ show a ++ " " ++ show b
--     show (Large _ a b) = "Large " ++ show a ++ " " ++ show b
--     show (SmallEq _ a b) = "SmallEq " ++ show a ++ " " ++ show b
--     show (LargeEq _ a b) = "LargeEq " ++ show a ++ " " ++ show b
--     show (Add _ a b) = "Add " ++ show a ++ " " ++ show b
--     show (Sub _ a b) = "Sub " ++ show a ++ " " ++ show b
--     show (Mul _ a b) = "Mul " ++ show a ++ " " ++ show b    
--     show (Div _ a b) = "Div " ++ show a ++ " " ++ show b
--     show (Neg _ a) = "Neg " ++ show a
--     show (Address _ a) = "Address " ++ show a
--     show (Pointer _ a) = "Pointer " ++ show a                         
--     show (Array _ a b) = "Array " ++ show a ++ " " ++ show b
--     show (Id _ a) = "Id " ++ a
--     show (Const _ a) = "Const " ++ (show a)
--     show (Paren _ a) = "Paren " ++ (show a)
--     show (ManyExp _ e) = "ManyExp " ++ ((intercalate " ") $ map show e)
--     show (Func _ s e) = "Func " ++ s ++ " " ++ ((intercalate " ") $ map show e)


---------------------------------
data Type = Int SourcePos
          | Void SourcePos
            deriving Show
            
-- instance Show Type where
--     show (Int _) = "Int"
--     show (Void  _)= "Void"
                     
-- Showのinstance宣言は、()をつけるのがめんどくさくなったので中止
