module Address where

import Control.Monad.State
import SemanticTypeDef
import IrTypeDef
import Ir
import AddrTypeDef

convertDecl :: Decl -> WithFp AddrDecl
convertDecl (Decl str k semtype) = do --declを受け取って
  fp <- get -- 現在のfpの状況をとって
  modify (+4) -- 4増やして
  return (AddrDecl str k semtype (fp + 4)) --4増やした値をdeclにくっつけて返す

convertDeclNum :: Decl -> Int -> WithFp AddrDecl
convertDeclNum (Decl str k semtype) num = do --declを受け取って
  return (AddrDecl str k semtype num) --4増やした値をdeclにくっつけて返す

  
makeAddr :: IrAST -> WithFp AddrAST
makeAddr ir = mapM makeAddrEx ir

makeAddrEx :: IrExternal -> WithFp AddrEx
makeAddrEx (VarDecl d) = do
  dec <- (convertDecl d)
  return (AddrVarDecl dec)
makeAddrEx (FunDef d args internal) = do
  modify (+ (2 * 4)) -- raとfpの分を開けておく
  inter <- mapM makeAddrIn internal -- 局所変数を変換する
  addrArgs <- mapM convertDecl args -- 引数を変換する
  num <- get -- 全部で使った分のメモリを取り出す
  dec <- (convertDeclNum d num)-- その分のメモリ(即ち最初に引くspのサイズ)をfuncのdeclのfpに入れる
  return (AddrFunDef dec addrArgs inter)
         
makeAddrIn :: IrInternal -> WithFp AddrIn
makeAddrIn _ = AddrVReturn
  
