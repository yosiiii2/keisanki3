module RemoveSugar (removeSugar) where

import Text.Parsec
import ParserFunc as P
import TypeDef -- my module                         

printDef :: External
printDef = case parse P.program "" "void print (int i);"of
          Right val -> (head val)
        

removeSugar :: [External] -> [External]
removeSugar ast = printDef : (map removeSugarExternal ast)
             
removeSugarExternal :: External -> External
removeSugarExternal (FunctionDef p t f s) = (FunctionDef p t f (removeSugarStmt s))
removeSugarExternal a = a

removeSugarStmt :: Stmt -> Stmt
removeSugarStmt (Statement p e) = (Statement p (removeSugarExp e))
removeSugarStmt (CompStmt p e s) = (CompStmt p e (map removeSugarStmt s))
removeSugarStmt (If p e s) = (IfElse p (removeSugarExp e) (removeSugarStmt s) (SemiOnly p))
removeSugarStmt (IfElse p e s1 s2) = (IfElse p (removeSugarExp e) (removeSugarStmt s1) (removeSugarStmt s2))
removeSugarStmt (While p e s) = (While p (removeSugarExp e) (removeSugarStmt s))
removeSugarStmt (For p1 e1 e2 e3 (CompStmt p2 e s)) = (CompStmt p1 [] [(Statement p1 e1),(While p1 (removeSugarExp e2) (CompStmt p2 e (s ++ [(Statement p1 e3)])))])
removeSugarStmt (For p e1 e2 e3 s) = (CompStmt p [] [(Statement p e1),(While p (removeSugarExp e2) (CompStmt p [] (map removeSugarStmt [s,(Statement p (removeSugarExp e3))])))])
removeSugarStmt (Return p e) = (Return p (removeSugarExp e))
removeSugarStmt a = a
                              
removeSugarExp :: Exp -> Exp
removeSugarExp (Assign p e1 e2) = (Assign p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Or p e1 e2) = (Or p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (And p e1 e2) = (And p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Equal p e1 e2) = (Equal p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (NotEqual p e1 e2) = (NotEqual p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Small p e1 e2) = (Small p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Large p e1 e2) = (Large p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (SmallEq p e1 e2) = (SmallEq p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (LargeEq p e1 e2) = (LargeEq p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Add p e1 e2) = (Add p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Sub p e1 e2) = (Sub p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Mul p e1 e2) = (Mul p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Div p e1 e2) = (Div p (removeSugarExp e1) (removeSugarExp e2))
removeSugarExp (Neg p e) = (Sub p (Const p 0) (removeSugarExp e))
removeSugarExp (Func p e1 e2) = (Func p (removeSugarExp e1) (map removeSugarExp e2))
removeSugarExp (Paren p e) = (Paren p (removeSugarExp e))
removeSugarExp (Address _ (Pointer _ e)) = (removeSugarExp e)
removeSugarExp (Address p e) = (Address p (removeSugarExp e))
removeSugarExp (Pointer p e) = (Pointer p (removeSugarExp e))
removeSugarExp (Array p e1 e2) = (Pointer p (Add p (removeSugarExp e1) (removeSugarExp e2)))
removeSugarExp (ManyExp p e) = (ManyExp p (map removeSugarExp e))
removeSugarExp a = a

                                  
