module ConvertToDecl (exchange) where

import TypeDef
import SemanticTypeDef

type FuncOr = Bool
    
-- Declのリストを受け取り、各DeclのSemTypeのリストかNothingを返す
collectParam :: [Decl] -> Maybe [SemType]
collectParam [] = Nothing
collectParam x = Just (map (\ (Decl _ _ st) -> st) x)

-- ASTを受け取り、変数宣言や関数定義などをDeclに書き換えたものを返す関数
exchange :: AST -> SAST
exchange a = map exchangeEx a

exchangeEx :: External -> SExternal
exchangeEx (FuncProto p st b s dl) =
    let hoge = map exchangeParm dl
    in (SFuncProto p (Decl s Proto (exchangeType st b (-1) (collectParam hoge) True)) hoge )
exchangeEx (FunctionDef p st b str dl s) =
    let hoge = map exchangeParm dl
    in (SFunctionDef p (Decl str Fun (exchangeType st b (-1) (collectParam hoge) True)) hoge (exchangeStmt s))
exchangeEx (Declaration p dl) = (SDeclaration p (map exchangeDec dl))

exchangeDec :: Declarator -> Decl
exchangeDec (Declarator _ st str b num) = (Decl str Var (exchangeType st b num Nothing False))

exchangeParm :: Declarator -> Decl
exchangeParm (Declarator _ st str b num) = (Decl str Parm (exchangeType st b num Nothing False))

exchangeType :: Type -> PointOr -> ArrayNum -> Maybe [SemType] -> FuncOr -> SemType
exchangeType (Void _) _ _ (Just a) True = SFunc SVoid a
exchangeType (Void _) _ _ (Nothing) True = SFunc SVoid []
exchangeType (Void _) _ _ _ False = SVoid
exchangeType (Int _) False (-1) (Just a) True = SFunc SInt a
exchangeType (Int _) True (-1) (Just a) True = SFunc (SPointer SInt) a
exchangeType (Int _) False (-1) Nothing True = SFunc SInt []
exchangeType (Int _) True (-1) Nothing True = SFunc (SPointer SInt) []
exchangeType (Int _) False (-1) (Nothing) False = SInt
exchangeType (Int _) True (-1) (Nothing) False = (SPointer SInt)
exchangeType (Int _) False num (Nothing) False = SArray SInt num
exchangeType (Int _) True num (Nothing) False= SArray (SPointer SInt) num
                                       
exchangeStmt :: Stmt -> SStmt
exchangeStmt (CompStmt p e s) = (SCompStmt p (map exchangeEx e) (map exchangeStmt s))
exchangeStmt (Statement p e) = (SStatement p e)
exchangeStmt (SemiOnly p) = (SSemiOnly p)
exchangeStmt (IfElse p e s1 s2) = (SIfElse p e (exchangeStmt s1) (exchangeStmt s2))
exchangeStmt (While p e s) = (SWhile p e (exchangeStmt s))
exchangeStmt (Return p e) = (SReturn p e)
exchangeStmt (VReturn p) = (SVReturn p)

