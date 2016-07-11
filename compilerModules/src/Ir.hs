module Ir(makeIr) where

import qualified Data.Map as M -- Map(連想配列)を扱うのに必要
import SemanticTypeDef
import IrTypeDef
import ObjectCollect
import Control.Monad.State

env :: (Env,Int,Int) -> Env
env (e,_,_) = e

varNum :: (Env,Int,Int) -> Int
varNum (_,v,_) = v

labelNum :: (Env,Int,Int) -> Int
labelNum (_,_,l) = l

addEmpEnv :: (Env,Int,Int) -> (Env,Int,Int)
addEmpEnv s = ((M.empty : env s), varNum s, labelNum s)

rmTopEnv ::  (Env,Int,Int) -> (Env,Int,Int)
rmTopEnv ((a:hoge),num,label) = (hoge,num,label)

updEnv :: Decl -> (Env,Int,Int) -> (Env,Int,Int)
updEnv d s = ((addEnv d (env s)),(varNum s),(labelNum s))

updVar ::  (Env,Int,Int) -> (Env,Int,Int)
updVar s = ((env s),(varNum s)+1,(labelNum s))             

updLabel ::  (Env,Int,Int) -> (Env,Int,Int)
updLabel s = ((env s),(varNum s),(labelNum s)+1)             
           
newVar :: Int -> String
newVar num = "_x" ++ (show num) 
         
newLabel :: Int -> String
newLabel num = "L" ++ (show num)

mkNewDecl :: WithIr Decl
mkNewDecl = do
  s <- get
  let str = newVar(varNum s) -- 新しい変数名を作る
  modify updVar
  return (Decl str Var STemp) -- その名前のdeclを作って返す

mkNewLabel :: WithIr IrInternal
mkNewLabel = do
  s <- get
  let str = newLabel (labelNum s)
  modify updLabel
  return (IrLabel str)

    
makeIr :: SAST -> WithIr IrAST
makeIr ast = do  
  hoge <- (mapM makeIrExternal ast)
  return $ concat hoge

irObjColDecl :: Decl -> WithIr IrExternal -- declを受け取って、varDeclにして返す
irObjColDecl d = do
  modify (updEnv d)  -- 環境に追加して
  return (VarDecl d) -- 受け取ったdeclをVarDeclに包んでreturn
         
makeIrExternal :: SExternal -> WithIr [IrExternal]
makeIrExternal (SDeclaration _ ds) = do -- DeclarationはまとめてvarDeclにする
  mapM irObjColDecl ds 
makeIrExternal (SFunctionDef _ d ds stmt) = do
  modify (updEnv d) -- 関数そのものを環境に追加
  modify addEmpEnv -- 環境の先頭に空のMap(Declを格納する)を追加
  parm <- mapM irObjColDecl ds--  パラメータを環境に追加
  st <- stmtToInternal stmt  -- stmt部分をIrに変換して、
  modify rmTopEnv -- topの環境を削除して
  return [(FunDef d parm st)]  -- FunDefにして返す
makeIrExternal (SFuncProto _ d args) = do
  modify (updEnv d) -- 環境にだけ入れておく -- だけじゃない
  modify addEmpEnv -- 環境の先頭に空のMap(Declを格納する)を追加
  parm <- mapM irObjColDecl args -- パラメータを環境に追加すると同時に変換する
  modify rmTopEnv -- topの環境を削除して          
  return [(FunDef d parm [])] -- プロトタイプ宣言はもういらない -- まだ要る

stmtToInternal :: SStmt -> WithIr [IrInternal]
stmtToInternal (SSemiOnly _) = return []
stmtToInternal (SCompStmt _ [] ss) = do -- 宣言が空なら
  stmts <- (mapM stmtToInternal ss) 
  return (concat stmts) -- ただのstmtの配列に潰す
stmtToInternal (SCompStmt _ ds ss) = do --宣言があったら
  modify addEmpEnv -- 空の環境を追加して
  decls <- (mapM makeIrExternal ds) -- declを変換して追加し、
  stmts <- (mapM stmtToInternal ss) -- 中身のstmtを変換して
  modify rmTopEnv -- 追加した環境を消して
  return [IrComp (concat decls) (concat stmts)] -- 返す
stmtToInternal (SVReturn _) = return [(IrVReturn)]
stmtToInternal (SStatement _ e) = do
  d <- mkNewDecl -- 新しい変数を作って
  let (SemanticExpression expr) = e
  expToInternal d expr -- その変数と中身を投げとく
stmtToInternal (SIfElse _ e s1 s2) = do
  dec <- mkNewDecl
  let (SemanticExpression expr) = e
  ex <- expToInternal dec expr
  l1 <- mkNewLabel
  l2 <- mkNewLabel
  stm1 <- stmtToInternal s1
  stm2 <- stmtToInternal s2
  return [(IrComp [VarDecl dec] (ex ++ [(IrIf dec l1 l2)] ++ [l1] ++ stm1 ++[l2] ++ stm2))]
stmtToInternal (SWhile _ e st) = do
  dec <- mkNewDecl
  let (SemanticExpression expr) = e
  ex <- expToInternal dec expr
  l1 <- mkNewLabel
  l2 <- mkNewLabel
  stm <- stmtToInternal st
  return [(IrComp [(VarDecl dec)] (ex ++ [(IrIf dec l1 l2)] ++ [l1] ++ stm ++[l2]))]
stmtToInternal (SReturn _ e) = do
  dec <- mkNewDecl
  let (SemanticExpression expr) = e
  ex <- expToInternal dec expr
  return [(IrComp [(VarDecl dec)] (ex ++ [(IrReturn dec)]))]


expToInternal :: Decl -> SExp -> WithIr [IrInternal]
-- 論理演算子
expToInternal d (SOr _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  l1 <- mkNewLabel
  l2 <- mkNewLabel
  l3 <- mkNewLabel
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                    (ex1 ++ ex2 ++ [(IrIf d1 l1 l2),l2,(IrIf d2 l1 l3),l1,(IrAssign d (IrLit 1)),l3,(IrAssign d (IrLit 0))]))]
expToInternal d (SAnd _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  l1 <- mkNewLabel
  l2 <- mkNewLabel
  l3 <- mkNewLabel
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                    (ex1 ++ ex2 ++ [(IrIf d1 l1 l3),l1,(IrIf d2 l2 l3),l2,(IrAssign d (IrLit 1)),l3,(IrAssign d (IrLit 0))]))]
-- 比較演算子
expToInternal d (SEqual _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b        
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrEQ d1 d2)]))]
expToInternal d (SNotEqual _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b        
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrNE d1 d2)]))]
expToInternal d (SSmall _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b        
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrST d1 d2)]))]
expToInternal d (SLarge _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b        
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrGT d1 d2)]))]
expToInternal d (SSmallEq _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b        
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrSE d1 d2)]))]
expToInternal d (SLargeEq _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b        
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrGE d1 d2)]))]
-- 算術演算子
        
expToInternal d (SAdd _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  if( (addCheck $ ty a) && (addCheck $ ty b) )
  then do -- aもbもpointerなら
    ex1 <- expToInternal d1 a                            
    ex2 <- expToInternal d2 b --そのまま
    return [(IrComp [(VarDecl d1),(VarDecl d2)]          
             (ex1 ++ ex2 ++ [IrAssign d (IrAdd d1 d2)]))]
  else if(addCheck $ ty a)
       then do -- aだけpointerなら
         ex1 <- expToInternal d1 a                            
         ex2 <- expToInternal d2 (SMul SInt b (SConst SInt 4)) --bを4倍する
         return [(IrComp [(VarDecl d1),(VarDecl d2)]          
                  (ex1 ++ ex2 ++ [IrAssign d (IrAdd d1 d2)]))]
       else if(addCheck $ ty b)
            then do -- bだけpointerなら
              ex1 <- expToInternal d1 (SMul SInt a (SConst SInt 4)) -- aを4倍する
              ex2 <- expToInternal d2 b                                 
              return [(IrComp [(VarDecl d1),(VarDecl d2)]               
                       (ex1 ++ ex2 ++ [IrAssign d (IrAdd d1 d2)]))]     
            else do -- どっちも違うなら         
              ex1 <- expToInternal d1 a                                           
              ex2 <- expToInternal d2 b
              return [(IrComp [(VarDecl d1),(VarDecl d2)] -- そのまま足す
                       (ex1 ++ ex2 ++ [IrAssign d (IrAdd d1 d2)]))]

        
-- expToInternal d (SAdd _ a b) = do
--   d1 <- mkNewDecl
--   d2 <- mkNewDecl
--   case (ty a) of
--     (SPointer _)  -> do -- aがpointerなら
--            ex1 <- expToInternal d1 a                            
--            ex2 <- expToInternal d2 (SMul SInt b (SConst SInt 4)) --bを4倍する
--            return [(IrComp [(VarDecl d1),(VarDecl d2)]          
--                     (ex1 ++ ex2 ++ [IrAssign d (IrAdd d1 d2)]))]               
--     _ -> do -- aがpointerでなくて
--       case (ty b) of 
--         (SPointer _) -> do -- bがpointerなら
--                     ex1 <- expToInternal d1 (SMul SInt a (SConst SInt 4)) -- aを4倍する
--                     ex2 <- expToInternal d2 b                                 
--                     return [(IrComp [(VarDecl d1),(VarDecl d2)]               
--                              (ex1 ++ ex2 ++ [IrAssign d (IrAdd d1 d2)]))]     
--         _ -> do -- どっちも違うなら         
--           ex1 <- expToInternal d1 a                                           
--           ex2 <- expToInternal d2 b
--           return [(IrComp [(VarDecl d1),(VarDecl d2)] -- そのまま足す
--                    (ex1 ++ ex2 ++ [IrAssign d (IrAdd d1 d2)]))]

expToInternal d (SSub _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- if ((addCheck $ ty a) && (not $ addCheck $ ty b)) -- aがpointerでbが違うなら
         then expToInternal d2 (SMul SInt b (SConst SInt 4)) -- bを4倍
         else expToInternal d2 b -- そうじゃないならそのまま
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrSub d1 d2)]))]
expToInternal d (SMul _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b        
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrMul d1 d2)]))]
expToInternal d (SDiv _ a b) = do
  d1 <- mkNewDecl
  d2 <- mkNewDecl
  ex1 <- expToInternal d1 a
  ex2 <- expToInternal d2 b        
  return [(IrComp [(VarDecl d1),(VarDecl d2)]
                  (ex1 ++ ex2 ++ [IrAssign d (IrDiv d1 d2)]))]
-- 変数周り
expToInternal d (SAddress _ a) = do
  s <- get -- stateを取って
  let str = newVar(varNum s) -- 新しい変数名を作る
      d2 = (Decl str Var STemp) -- その名前のdeclを作って
      var = (VarDecl d2) -- そのvarDeclを作る -- compstmt開始
  modify addEmpEnv -- 空の環境を作成
  modify (updEnv d2) -- 環境に追加
  modify updVar -- 変数名に使う数字を更新して
  int <- expToInternal d2 a -- addressの中身をd2に束縛
  let re = (IrRead d d2) -- d2を受け取ったdに束縛して
  return ([(IrComp [var] (int ++ [re]))]) -- addressの中身を束縛する過程とdに束縛した式をまとめて返す
expToInternal d (SPointerExp _ a) = do
  dec <- mkNewDecl -- 新しい変数を作る
  ir <- expToInternal dec a -- そこに、Pointerの中身を束縛する
  return [IrComp [(VarDecl dec)]
                 (ir ++ [(IrRead d dec)])] -- 中身を束縛する過程と、IrReadをくっつけて返す
expToInternal d (SId _ a) =  do
  s <- get
  let e = env s      
      dec = objCheck a e
      (Just decl) = dec
  return [(IrAssign d (IrVar decl))]
expToInternal d (SConst _ a) = return [(IrAssign d (IrLit a))]

-- その他
expToInternal d (SManyExp _ e) = do
  ds <- manyDecls e
  inter <- (zipInternal ((init ds) ++ [d]) e)
  return [IrComp (map (VarDecl) ds)
                 inter]
expToInternal d (SFuncExp _ str es) = do
  if(str == "print")
  then do
    dec <- mkNewDecl
    let ex =  head es
    inter <- expToInternal dec ex
    return [IrComp [(VarDecl dec)] (inter ++ [IrPrint dec])]
  else do
    sta <- get -- 状態を取得
    let e = env sta -- そこから環境だけ取り出し
        dec = objCheck str e --関数のDeclを検索
        (Just decl) = dec --それをdeclに束縛         
    if(ftype (t decl) == SVoid)
    then do
      decls <- manyDecls es
      inters <- zipInternal decls es
      return [IrComp (map (VarDecl) decls) (inters ++ [(IrVCall decl decls)])]
    else do 
      decls <- manyDecls es
      inters <- zipInternal decls es
      return [IrComp (map (VarDecl) decls) (inters ++ [(IrCall d decl decls)])]
expToInternal _ (SAssign _ e1 e2) = do -- こういうところでdeclを読み捨ててるからちょくちょく番号が抜ける
  st <- get
  case e1 of
    (SId _ str) -> do
            let (Just d) = objCheck str (env st)
            d1 <- mkNewDecl
            ex1 <- expToInternal d1 e2
            return ([(IrComp [(VarDecl d1)]
                             (ex1 ++ [IrAssign d (IrVar d1)]))])
    (SPointerExp _ e) -> do
            d1 <- mkNewDecl -- 新しい変数を作って
            d2 <- mkNewDecl
            inter <- expToInternal d1 e -- そこに中身を束縛
            right <- expToInternal d2 e2
            return [(IrComp [(VarDecl d1),(VarDecl d2)]
                            (inter ++ right ++ [(IrWrite d1 d2)]))]

-- pointerとaddrが間違ってる？？
-- あと、メモリの足し算
-- あと、assign.
-- caseで分けて各々の処理を書く？

manyDecls :: [a] -> WithIr [Decl]
manyDecls [] = return []
manyDecls (_:xs) = do
  dc <- mkNewDecl
  ds <- manyDecls xs
  return (dc:ds)

zipInternal :: [Decl] -> [SExp] -> WithIr [IrInternal]
zipInternal _ [] = return []
zipInternal [] _ = return []
zipInternal (x:xs) (e:es) = do
  hoge <- expToInternal x e
  fuga <- zipInternal xs es
  return (hoge ++ fuga)


-- Pointerの類ならTrue
addCheck :: SemType -> Bool
addCheck (SPointer _) = True
addCheck (SArray _ _ ) = True
addCheck (SFunc ty _) = (addCheck ty)
addCheck _ = False

        
