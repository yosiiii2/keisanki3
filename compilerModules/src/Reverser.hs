module Reverser (reverser) where

import TypeDef
import Data.List

setSemi :: String -> String
setSemi s = s ++ ";"
    
reverser :: AST -> String
reverser e = intercalate " " (map reverseExternal e)

reverseExternal :: External -> String
reverseExternal (FuncProto _ t True s d) = (reverseType t) ++" "++ " *" ++ s ++"(" ++ intercalate "," (map reverseDeclarator d) ++")"++ ";"
reverseExternal (FuncProto _ t False s d) = (reverseType t) ++" "++ s ++  "(" ++intercalate ", "(map reverseDeclarator d) ++ ")" ++ ";"
reverseExternal (FunctionDef _ t False str d stmt) = (reverseType t) ++" "++ str ++"("++ intercalate ", "(map reverseDeclarator d) ++ ")" ++ (reverseStmt stmt)
reverseExternal (FunctionDef _ t True str d stmt) = (reverseType t) ++" "++ " *" ++ str ++"("++ intercalate ", "(map reverseDeclarator d) ++ ")"++ (reverseStmt stmt)
reverseExternal (Declaration _ d) = intercalate " " (map setSemi (map reverseDeclarator d))

reverseDeclarator :: Declarator -> String
reverseDeclarator (Declarator _ t s False (-1)) = (reverseType t) ++" "++ s
reverseDeclarator (Declarator _ t s True (-1)) = (reverseType t) ++" "++ "*" ++ s
reverseDeclarator (Declarator _ t s False n) = (reverseType t) ++ " "++s ++ "[" ++ (show n) ++"]"
reverseDeclarator (Declarator _ t s True n) = (reverseType t) ++ " "++ "*" ++ s ++ "[" ++ (show n) ++"]"

                                                   
reverseStmt :: Stmt -> String
reverseStmt (Statement _ e) = (reverseExp e) ++ ";"
reverseStmt (SemiOnly _) = ";"
reverseStmt (CompStmt _ e s) = "{" ++ (intercalate "" (map reverseExternal e)) ++(intercalate ""(map reverseStmt s)) ++"}"
reverseStmt (If _ e s) = "if(" ++(reverseExp e) ++ ")" ++ (reverseStmt s)
reverseStmt (IfElse _ e s1 s2) = "if(" ++ (reverseExp e) ++ ")" ++ (reverseStmt s1) ++ "else" ++ (reverseStmt s2)
reverseStmt (While _ e s) = "while(" ++ (reverseExp e) ++ ")" ++ (reverseStmt s)
reverseStmt (For _ e1 e2 e3 s) = "for("++(reverseExp e1) ++ ";" ++ (reverseExp e2)++";"++(reverseExp e3)++")"++(reverseStmt s)
reverseStmt (Return _ e) = "return " ++ (reverseExp e) ++ ";"
reverseStmt (VReturn _) = "return " ++ ";"


reverseExp :: Exp -> String
reverseExp (Assign _ e1 e2) = (reverseExp e1) ++ " = " ++ (reverseExp e2)
reverseExp (Or _ e1 e2) = (reverseExp e1) ++ " || " ++ (reverseExp e2)
reverseExp (And _ e1 e2) = (reverseExp e1) ++ " && " ++ (reverseExp e2)
reverseExp (Equal _ e1 e2) = (reverseExp e1) ++ " == " ++ (reverseExp e2)
reverseExp (NotEqual _ e1 e2) = (reverseExp e1) ++ " != " ++ (reverseExp e2)
reverseExp (Small _ e1 e2) = (reverseExp e1) ++ " < " ++ (reverseExp e2)
reverseExp (Large _ e1 e2) = (reverseExp e1) ++ " > " ++ (reverseExp e2)
reverseExp (SmallEq _ e1 e2) = (reverseExp e1) ++ " <= " ++ (reverseExp e2)
reverseExp (LargeEq _ e1 e2) = (reverseExp e1) ++ " >= " ++ (reverseExp e2)
reverseExp (Add _ e1 e2) = (reverseExp e1) ++ " + " ++ (reverseExp e2)
reverseExp (Sub _ e1 e2) = (reverseExp e1) ++ " - " ++ (reverseExp e2)
reverseExp (Mul _ e1 e2) = (reverseExp e1) ++ " * " ++ (reverseExp e2)
reverseExp (Div _ e1 e2) = (reverseExp e1) ++ " / " ++ (reverseExp e2)
reverseExp (Neg _ e) = "-" ++ (reverseExp e)
reverseExp (Address _ e) = "&" ++ (reverseExp e)
reverseExp (Pointer _ e) = "*" ++ (reverseExp e)
reverseExp (Array _ e1 e2) = (reverseExp e1) ++ "[" ++ (reverseExp e2) ++"]"
reverseExp (Func _ e1 e2) = e1 ++ "(" ++ (show $ map reverseExp e2) ++ ")"
reverseExp (Id _ e) = e
reverseExp (Const _ e) = (show e)
reverseExp (Paren _ e) = "(" ++ (reverseExp e) ++ ")"
reverseExp (ManyExp _ e) = intercalate "," (map reverseExp e)
                            
reverseType :: Type -> String
reverseType (Int _) = "int"
reverseType (Void _) = "void"

