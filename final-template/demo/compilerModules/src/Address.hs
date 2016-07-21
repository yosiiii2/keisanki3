module Address (makeAddr) where

import qualified Data.Map as M
import Control.Monad.State
import SemanticTypeDef
import ObjectCollect
import IrTypeDef
import AddrTypeDef


env :: (Env,Integer,Integer) -> Env
env (e,_,_) = e

fp :: (Env,Integer,Integer) -> Integer
fp (_,i,_) = i

maxFp :: (Env,Integer,Integer) -> Integer
maxFp (_,_,i) = i

updFp :: Integer -> (Env,Integer,Integer) -> (Env,Integer,Integer)
updFp num (e,fpNum,m) = (e,fpNum-(4*num),m)

updMax :: Integer -> (Env,Integer,Integer) -> (Env,Integer,Integer)
updMax num (e,fpNum,_) = (e,fpNum,num)

updEnv :: Decl -> (Env,Integer,Integer) -> (Env,Integer,Integer)
updEnv d s = ((addEnv d (env s)),(fp s),(maxFp s))

addEmpEnv :: (Env,Integer,Integer) -> (Env,Integer,Integer)
addEmpEnv s = ((M.empty : env s), fp s, maxFp s)

rmTopEnv :: (Env,Integer,Integer) -> (Env,Integer,Integer)
rmTopEnv ((_:xs),f,m) = (xs,f,m)

convertFuncDecl :: Integer -> Decl -> WithFp Decl
convertFuncDecl num d = return (AddrDecl (name d) (kind d) (t d) num)

convertDeclNum :: Integer -> Decl -> WithFp Decl
convertDeclNum hoge (Decl str k semtype) = do --declと要素数を受け取って
  st <- get -- 現在の状況をとって
  let nowFp = fp st -- 今のfpを取る
      ret = (AddrDecl str k semtype nowFp) -- 返すモノを用意して
  modify (updEnv ret) -- declを環境に追加して
  modify (updFp hoge) -- 要素数*4分fpをずらして
  return ret --返す

convertDecl :: Decl -> WithFp Decl
convertDecl d = convertDeclNum 1 d -- 要素数一個の時のDeclの変換

convertDeclGlobal :: Decl -> WithFp Decl
convertDeclGlobal d = do
  let ret = (AddrDecl (name d) (kind d) (t d) (-1)) -- global変数はfpの値が-1
  modify (updEnv ret)
  return ret


makeAddr :: IrAST -> WithFp AddrAST
makeAddr ir = mapM makeAddrEx ir

makeAddrEx :: IrExternal -> WithFp AddrEx
makeAddrEx (VarDecl d) = do -- globalのVarDeclの変換
  modify (\(e,_,_) -> (e,0,0)) -- 一回fpを0にする
  ret <- convertDeclGlobal d -- とりあえずfpには0を入れておく
  return (AddrVarDeclGlobal ret)
makeAddrEx (FunDef d args internal) = do
  modify (updEnv d) -- 関数を環境にいれる(この時点では旧Declでofsなし)
  modify (\(e,_,_) -> (e,0,0)) -- 一回fpを0にする(パラメータを0スタートで置くため),maxも0にする
  modify addEmpEnv -- param用に環境を作る
  param <- makeAddrVarParam args -- パラメータを変換する
  modify (\(e,_,_) -> (e,-4,0)) -- fp=-4(局所変数は-4から)maxも0にする(パラメータの分は無視する)
  inter <- mapM makeAddrIn internal -- 局所変数を変換する
  st <- get -- 局所変数全体で使った分の最大のfp(= maxFp)を取り出す
  let num = maxFp st
  dec <- (convertFuncDecl num d)-- その分のメモリ(即ち最初に引くspのサイズからパラメータの分を引いたもの)をfuncのdeclのfpに入れる -- それ用のconverter
  modify rmTopEnv -- param用に作った環境を潰す
  modify (\(e,f,_) -> (e,f,0)) -- maxを0に戻す
  return (AddrFunDef dec param (concat inter))

makeAddrVarLocal :: IrExternal -> WithFp AddrIn -- 局所変数のVarDeclの変換
makeAddrVarLocal (VarDecl d) = do
  ret <- case (t d) of
           (SArray _ num) -> (convertDeclNum num d) --配列やったら要素数分とる
           _ -> convertDecl d -- それ以外やったら一個取る
  st <- get
  let nowMax = maxFp st
      nowfp = fp st
  if(nowfp < nowMax)
    then modify (updMax nowfp)
    else modify (updMax nowMax)
  return (AddrVarDecl ret)

makeAddrVarParam :: [IrExternal] -> WithFp [AddrIn] -- パラメータのVarDeclの変換
makeAddrVarParam [] = return []
makeAddrVarParam ((VarDecl d):ds) = do
  ret <- convertDeclNum (-1) d-- +方向に取るために-1する
  rest <- ((makeAddrVarParam ds))
  return ((AddrVarDecl ret) : rest)

makeAddrIn :: IrInternal -> WithFp [AddrIn]
makeAddrIn (IrAssign d irin) = do
  st <- get
  let (Just dec) = objCheck (name d) (env st)
  ain <- makeAddrIn irin
  return [(AddrAssign dec (head ain))]
makeAddrIn (IrWrite d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrWrite dec1 dec2]
makeAddrIn (IrRead d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrRead dec1 dec2]
makeAddrIn (IrLabel str) = return [(AddrLabel str)]
makeAddrIn (IrIf d in1 in2) = do
  st <- get
  let (Just dec) = objCheck (name d) (env st)
  adIn1 <- makeAddrIn in1
  adIn2 <- makeAddrIn in2
  return [(AddrIf dec (head adIn1) (head adIn2))]
makeAddrIn (IrGoto in1) = do
  adIn <- makeAddrIn in1
  return [(AddrGoto (head adIn))]
makeAddrIn (IrCall d1 d2 argsd) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
      (Just hoge) = mapM (\d -> (objCheck (name d) (env st))) argsd
  return [AddrCall dec1 dec2 hoge]
makeAddrIn (IrVCall d argsd) = do
  st <- get
  let (Just dec) = objCheck (name d) (env st)
      (Just hoge) = mapM (\fuga -> (objCheck (name fuga) (env st))) argsd
  return [AddrVCall dec hoge]
makeAddrIn (IrReturn d) = do
  st <- get
  let (Just dec) = objCheck (name d) (env st)
  return [AddrReturn dec]
makeAddrIn IrVReturn = return [(AddrVReturn)]
makeAddrIn (IrPrint d) = do
  st <- get
  let (Just dec) = objCheck (name d) (env st)
  return [AddrPrint dec]
makeAddrIn (IrComp e i) = do
  st <- get
  let nowFp = fp st -- 今のfpを取る
  modify addEmpEnv -- 空の環境を追加
  decls <- mapM makeAddrVarLocal e -- 宣言を変換して環境にいれる
  inter <- mapM makeAddrIn i -- 中身を変換
  modify (\(en,_,m) -> (en,nowFp,m)) -- fpを元に戻す(スコープ外れたら同じとこに置けるから)
  modify rmTopEnv -- 追加した環境を削除
  return (decls ++ concat inter) -- compの構造を潰して
makeAddrIn (IrAdd d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrAdd dec1 dec2]
makeAddrIn (IrSub d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrSub dec1 dec2]
makeAddrIn (IrMul d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrMul dec1 dec2]
makeAddrIn (IrDiv d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrDiv dec1 dec2]
makeAddrIn (IrGT d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrGT dec1 dec2]
makeAddrIn (IrST d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrST dec1 dec2]
makeAddrIn (IrGE d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrGE dec1 dec2]
makeAddrIn (IrSE d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrSE dec1 dec2]
makeAddrIn (IrEQ d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrEQ dec1 dec2]
makeAddrIn (IrNE d1 d2) = do
  st <- get
  let (Just dec1) = objCheck (name d1) (env st)
      (Just dec2) = objCheck (name d2) (env st)
  return [AddrNE dec1 dec2]
makeAddrIn (IrAddr d) = do
  st <- get
  let (Just dec) = objCheck (name d) (env st)
  return [AddrAddr dec]
makeAddrIn (IrVar d) = do
  st <- get
  let (Just dec) = objCheck (name d) (env st)
  return [AddrVar dec]
makeAddrIn (IrLit num) = return [(AddrLit num)]
