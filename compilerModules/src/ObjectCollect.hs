module ObjectCollect where

import Text.Parsec.Pos -- position 扱うのに必要
import TypeDef -- Expとかの定義(先頭にSがつかないdata型)
import qualified Data.Map as M -- Map(連想配列)を扱うのに必要
import Control.Monad.Except -- Exceptモナドを扱うのに必要
import Control.Monad.State -- Stateモナドを扱うのに必要
import Control.Monad.Writer    
import SemanticTypeDef -- SExternal とかの定義(先頭にSがつくやつとDecl)
import ConvertToDecl -- 各種宣言をDeclに置き換えたASTを吐く
    
-- キーに一致するdeclを探して、見つかったらそのdeclを返して、見つからなかったらNothingを返す
objCheck :: String -> Env -> Maybe Decl
objCheck _ [] = Nothing
objCheck s (x:xs) =
    let hoge = M.lookup s x
     in if(hoge == Nothing)
        then (objCheck s xs)
        else hoge


-- 受け取ったDeclを環境に追加
addEnv :: Decl -> Env -> Env
addEnv d [] = [(M.insert (name d) d M.empty)]
addEnv d (e:es) = ((M.insert (name d) d e) : es)


objCol :: AST -> WithEnv SAST
objCol a = mapM objColEx (exchange a)


-- SExternalからobjectを収集
objColEx ::  SExternal -> WithEnv SExternal
objColEx (SDeclaration p ds) = do -- SDeclarationを受け取ると
  decls <- mapM (objColDecl p) ds -- 中の各DeclにobjColDeclをかけた結果をdeclに束縛して
  return (SDeclaration p decls) -- SDeclarationにまとめて返す(ここで環境がつけ加わる)
objColEx (fp@(SFuncProto p d sd)) = do -- SFuncProtoを受け取ると
  env <- get -- 環境を取り出して
  let hoge = objCheck (name d) env -- その関数名を環境内で確認し
  if (hoge == Nothing) -- 存在しなければ
  then do modify (addEnv d) -- 環境に追加して
          modify (M.empty :) -- 新たに環境を作り
          mapM (objColDecl p) sd -- 引数の各DeclにobjColDeclをかけ
          modify (\(_:xs) -> xs) -- 新たに作った環境を消去し
          return fp -- 元のSFuncProtoを返す(ここで環境がつけ加わる)
  else let (Just (Decl _ k t2)) = hoge -- 存在すれば
        in if(((k == Proto)||(k == Fun))&&(t(d) == t2)) -- それがプロトタイプ宣言か関数宣言でありかつ引数の型が等しければ
           then return fp -- 環境をつけて元のSFuncProtoを返す
           else throwError ("error at " ++ (show (sourceLine p))++ (show (sourceColumn p)) ++ ":Double Declaraion") -- そうでなければerrorを吐く
objColEx (fp@(SFunctionDef p d sd stmt)) = do -- SFunctionDefを受け取ると
  env <- get -- 環境を取り出して
  let hoge = objCheck (name d) env -- 関数名を環境内で確認し
  if (hoge == Nothing) -- 存在しなければ
  then do modify (addEnv d)
          modify (M.empty :)
          mapM (objColDecl p) sd -- プロトタイプ宣言と同様にした上で
          objColStmt (t d) stmt -- 中のstatementに対してobjColStmtをかける
          modify (\(_:xs) -> xs)
          return fp 
  else let (Just (Decl _ k t2)) = hoge --存在すれば
        in if((k == Proto)&&(t(d) == t2)) -- それがプロトタイプ宣言であり且つ型が同じなら
           then do
             modify (M.empty :) -- 無視して続行する
             mapM (objColDecl p) sd
             objColStmt t2 stmt
             modify (\(_:xs) -> xs)
             return fp
           else throwError  ("error at " ++ (show (sourceLine p)) ++ (show (sourceColumn p)) ++ ":Double Declaration") --そうでなければerrorを吐く


objColDecl :: SourcePos -> Decl -> WithEnv Decl
-- positionとDeclを受け取り、元のDeclに環境を付け加えたものを返す関数
objColDecl p d = do
  if(((t d) == SVoid)&&((kind d) == Var || (kind d) == Parm))
  then throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":You can't declare Void variable or parameter:" ++ (name d))
  else do
    env <- get -- 環境を取り出して
    let hoge = objCheck (name d) env -- 環境を確認して
    if (hoge == Nothing) -- なかったら
    then do
      modify (addEnv d) -- 環境に追加して
      return d --受け取ったdeclをreturn
    else let (Just (Decl _ k2 _)) = hoge -- 既にされている定義が
         in  if(k2 == Proto || k2 == Fun) -- プロトタイプ宣言か関数定義であり
             then
                 if (length(env) == 1) -- 大域変数なら
                 then throwError ("error at " ++ (show (sourceLine p))++":"++ (show (sourceColumn p)) ++ ":Double Declaraion : " ++ (name d))  -- エラーを吐く
                 else do
                   modify (addEnv d) -- 違ったら環境に追加
                   return d
             else if(k2 == Var)
                  then -- 変数定義なら
                      if (M.member (name d) (head env)) -- 同じレベルなら
                      then throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Double Declaraion :" ++ (name d))  -- エラーを吐く
                      else do
                        modify (addEnv d) -- 違ったら環境に追加
                        return d
                  else do -- パラメータ宣言なら
                    if (M.member (name d) (head env)) -- 同じレベルなら
                    then throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Double Declaraion :" ++ (name d))  -- エラーを吐く
                    else do -- でなければ
                      tell [((show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++"Warning:" ++ "using same name between parameter and variable : " ++ (name d))] -- warningだけ吐いて
                      modify (addEnv d) -- 環境に追加
                      return d

                         
objColStmt :: SemType -> SStmt -> WithEnv SStmt -- 今いる関数の型としてのsとSStmtを受け取って環境付きのSStmtを返す
objColStmt _ (SStatement p e) = do -- 受け取ったのがSStatementなら
  objColExp e
  return (SStatement p e)
objColStmt ty (c@(SCompStmt _ xs st)) = do
  modify (M.empty :)
  mapM objColEx xs
  mapM (objColStmt ty) st
  modify (\(_:hoge) -> hoge)
  return c
objColStmt ty (SIfElse p e s1 s2) = do
  cond <- objColExp e
  if(cond == SInt)
  then do
    objColStmt ty s1
    objColStmt ty s2
    return (SIfElse p e s1 s2)
  else  throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":If statement's condition's type: " ++ (show cond) ++ " isn't Int.")
objColStmt ty (SWhile p e s1) = do
  objColExp e
  objColStmt ty s1
  return (SWhile p e s1)
objColStmt ty (SReturn p e) = do
  etype <- objColExp e
  if(ty == etype)
  then return (SReturn p e)
  else throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":return type:" ++ (show etype) ++ " doesn't match the function's type: " ++ (show ty) ++ ".")
objColStmt ty (SVReturn p) =
    if(ty == SVoid)
    then return (SVReturn p)
    else throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ "return type: Void doesn't match the function's type: " ++ (show ty) ++ ".")
objColStmt _ hoge = return hoge


idCheck :: SemType -> SemType
idCheck (SArray ty _) = (SPointer ty)
idCheck a = a
                    
-- expressionからオブジェクト名を引っ張ってこないと型の情報が取り出せない
-- じゃあ戻り値を型にすればいい
objColExp :: Exp -> WithEnv SemType
objColExp (Id p str) = do
  env <- get
  let hoge = objCheck str env
  if(hoge == Nothing) then throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":unknown variable " ++ str) 
  else do
      let (Just a) = hoge
      case (t a) of
        (SFunc _ _) -> throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":called function as variable " ++ str) 
        _ -> return (idCheck (t a))
objColExp (Const _ _) = return SInt
objColExp (Func p str args) = do
  env <- get
  let hoge = objCheck str env
  if(hoge == Nothing) then throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":unknown function " ++ str)
  else do
      let (Just a) = hoge
          test = (t a)
      case test of
        (SFunc _ typeOfArgs) -> do
                  typeOfArgs2 <- mapM objColExp args 
                  if(typeOfArgs == typeOfArgs2)
                  then (return test)
                  else throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":arguments' types don't match, in the call of function:" ++ str)                  
        _ -> throwError ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ": called function is not declared as function : " ++ (show a))
objColExp (Or p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if((t1==SInt) && (t2==SInt))
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Or operation's arguments' types don't match")
objColExp (And p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if((t1==SInt) && (t2==SInt))
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":And Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (Equal p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if(t1 == t2)
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Equal Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (NotEqual p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if(t1 == t2)
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":NotEqual Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (Small p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if(t1 == t2)
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Less Than Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (Large p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if(t1 == t2)
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ "Greater Than Operation's :arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (SmallEq p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if(t1 == t2)
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":LE Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (LargeEq p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if(t1 == t2)
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":GE Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (Add p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if((t1 == SInt)&&(t2 == SInt))
  then return SInt
  else if (((t1 == SInt)&&(t2 == (SPointer SInt)))||((t1 == (SPointer SInt))&&(t2 == SInt)))
       then return (SPointer SInt)
       else  if (((t1 == SInt)&&(t2 == (SPointer (SPointer SInt)))) || ((t1 == (SPointer (SPointer SInt)))&&(t2 == SInt)))
             then return (SPointer (SPointer SInt))
             else  throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Add Operation's arguments' types don't match " ++ (show t1) ++ " and " ++ (show t2))
objColExp (Sub p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if((t1 == SInt)&&(t2 == SInt))
  then return SInt
  else if ((t1 == (SPointer SInt))&&(t2 == SInt))
       then return (SPointer SInt)
       else  if ((t1 == (SPointer (SPointer SInt)))&&(t2 == SInt))
             then return (SPointer (SPointer SInt))
             else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Subtraction Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (Mul p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if((t1 == SInt)&&(t2 == SInt))
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Multiplication Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (Div p e1 e2) = do
  t1 <- (objColExp e1)
  t2 <- (objColExp e2)
  if((t1 == SInt)&&(t2 == SInt))
  then return SInt
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Division Operation's arguments' types don't match : " ++ (show t1) ++ " and " ++ (show t2))
objColExp (ManyExp _ e) = do
  let xs = map objColExp e
  x <- (last xs)
  return x
objColExp (Address p e) = do
  x <- objColExp e
  if(x == SInt)
  then case e of
         (Id _ _ ) -> return (SPointer SInt)
         _ -> throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":in the Address operator, you can only use Identifier.")            
  -- else if(x == (SPointer SInt)) --pointerのアドレスはナシ
  --      then return (SPointer (SPointer SInt))
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":in the Address, type don't match")            
objColExp (Pointer p e) = do
  x <- objColExp e
  if(x == (SPointer SInt))
  then return SInt
  else if(x == (SPointer (SPointer SInt)))
       then return (SPointer SInt)
       else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":in the Pointer, type don't match")
objColExp (Assign p l r) = do
  if(leftCheck l)
  then do
      left <- (objColExp l)
      kata <- (objColExp r)
      if(left == kata)
      then return left
      else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Assign types don't match " ++ (show left) ++ " and " ++ (show kata))
  else throwError  ("error at " ++ (show (sourceLine p)) ++ ":" ++ (show (sourceColumn p)) ++ ":Assign's left side needs variable")


leftCheck :: Exp -> Bool
leftCheck (Pointer _ _) = True
leftCheck (Id _ _) = True
leftCheck _ = False

