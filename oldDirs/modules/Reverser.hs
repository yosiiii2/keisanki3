module Reverser (reverser) where

import TypeDef
import Data.List

reverser :: [External] -> String
reverser e = intercalate "" (map reverseExternal e)
    
reverseExternal :: External -> String
reverseExternal (FuncProto _ t f) = (reverseType t) ++ " " ++ (reverseFunctionDec f) ++ ";"
reverseExternal (FunctionDef _ t f s) = (reverseType t) ++ " " ++ (reverseFunctionDec f) ++ (reverseStmt s)
reverseExternal (Declaration _ t d) = (reverseType t) ++ " " ++ (intercalate "," (map reverseDecralator d)) ++ ";"

reverseDecralator :: Decralator -> String
reverseDecralator (PointDec _ d) = "*" ++ (reverseDDec d)
reverseDecralator (Dec _ d) = (reverseDDec d)

reverseDDec :: DDec -> String                              
reverseDDec (DirectDec _ s) = s 
reverseDDec (ArrayDec _ s i) = s ++ "[" ++ (show i) ++"]"

reverseFunctionDec :: FunctionDec -> String
reverseFunctionDec (FuncDec _ s p) = s ++ "(" ++ intercalate "," (map reverseParameterDeclaration p) ++ ")"

reverseParameterDeclaration :: ParameterDeclaration -> String
reverseParameterDeclaration (ParaDec _ t s) = (reverseType t) ++ " " ++ s
reverseParameterDeclaration (PointParaDec _ t s) = "*" ++ (reverseType t) ++ " " ++ s

                                                   
reverseStmt :: Stmt -> String
reverseStmt (Statement _ e) = (reverseExp e) ++ ";"
reverseStmt (SemiOnly _) = ";"
reverseStmt (CompStmt _ e s) = "{" ++ (intercalate "" (map reverseExternal e)) ++(intercalate ""(map reverseStmt s)) ++"}"
reverseStmt (If _ e s) = "if(" ++(reverseExp e) ++ ")" ++ (reverseStmt s)
reverseStmt (IfElse _ e s1 s2) = "if(" ++ (reverseExp e) ++ ")" ++ (reverseStmt s1) ++ "else" ++ (reverseStmt s2)
reverseStmt (While _ e s) = "while(" ++ (reverseExp e) ++ ")" ++ (reverseStmt s)
reverseStmt (For _ e1 e2 e3 s) = "for("++(reverseExp e1) ++ ";" ++ (reverseExp e2)++";"++(reverseExp e3)++")"++(reverseStmt s)
reverseStmt (Return _ e) = "return " ++ (reverseExp e) ++ ";"


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
reverseExp (Func _ e1 e2) = (reverseExp e1) ++ "(" ++ (show $ map reverseExp e2) ++ ")"
reverseExp (Id _ e) = e
reverseExp (Const _ e) = (show e)
reverseExp (Paren _ e) = "(" ++ (reverseExp e) ++ ")"
reverseExp (ManyExp _ e) = intercalate "," (map reverseExp e)
                            
reverseType :: Type -> String
reverseType (Int _) = "int"
reverseType (Void _) = "void"

