module RemoveSugar (removeSugar) where

import Text.Parsec
import ParserFunc as P
import TypeDef 

-- printのプロトタイプ宣言のコードをそのままパースした結果をExternalとしておき、それをASTの先頭に挿入する
printDef :: External
printDef = case parse P.program "" "void print (int i);"of
          Right val -> (head val)
        
-- printのDefを挿入している以外は、statementに入るまでは一切触らない
removeSugar :: AST -> AST
removeSugar ast = printDef : (map removeSugarExternal ast)
             
removeSugarExternal :: External -> External
removeSugarExternal (FunctionDef p t b str d stmt) = (FunctionDef p t b str d (removeSugarStmt stmt))
removeSugarExternal a = a

-- IfとForは変換して、他は、中身のexpressionを変換する関数に投げる
removeSugarStmt :: Stmt -> Stmt
removeSugarStmt (If p e s) = (IfElse p (removeSugarExp e) (removeSugarStmt s) (SemiOnly p))
removeSugarStmt (For p1 e1 e2 e3 (CompStmt p2 e s)) = (CompStmt p1 [] [(Statement p1 (removeSugarExp e1)),(While p1 (removeSugarExp e2) (CompStmt p2 (map removeSugarExternal e) ((map removeSugarStmt s) ++ [(Statement p1 (removeSugarExp e3))])))])
removeSugarStmt (For p e1 e2 e3 st) = (CompStmt p [] [(Statement p (removeSugarExp e1)),(While p (removeSugarExp e2) (CompStmt p []  ([(removeSugarStmt st)] ++ [(Statement p (removeSugarExp e3))])))])
-- 以上二つが変換される
removeSugarStmt (Statement p e) = (Statement p (removeSugarExp e))
removeSugarStmt (CompStmt p e s) = (CompStmt p e (map removeSugarStmt s))
removeSugarStmt (IfElse p e s1 s2) = (IfElse p (removeSugarExp e) (removeSugarStmt s1) (removeSugarStmt s2))
removeSugarStmt (While p e s) = (While p (removeSugarExp e) (removeSugarStmt s))
removeSugarStmt (Return p e) = (Return p (removeSugarExp e))
removeSugarStmt (VReturn p) = (VReturn p)
removeSugarStmt (SemiOnly p) = (SemiOnly p)
-- removeSugarStmt a = a
-- forの中身がcompstmtじゃ無いと死ぬ


-- ASTの構造上Parenを構造として必要としないので削除し、配列の参照と単項演算子"-"を変換する

removeSugarExp :: Exp -> Exp
removeSugarExp (Paren _ e) = (removeSugarExp e)
removeSugarExp (Neg p e) = (Sub p (Const p 0) (removeSugarExp e))
removeSugarExp (Array p e1 e2) = (Pointer p (Add p (removeSugarExp e1) (removeSugarExp e2)))
removeSugarExp (Address p e) =
    let a = removeSugarExp e
    in case a of
      (Pointer _ ex) -> ex
      _ ->  (Address p a)
-- 以上4つが変換される
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
removeSugarExp (Func p str e2) = (Func p str (map removeSugarExp e2))
removeSugarExp (Pointer p e) = (Pointer p (removeSugarExp e))
removeSugarExp (ManyExp p e) = (ManyExp p (map removeSugarExp e))
removeSugarExp a = a

                                  
