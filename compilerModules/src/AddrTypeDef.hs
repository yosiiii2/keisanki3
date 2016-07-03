module AddrTypeDef where

import SemanticTypeDef
import Control.Monad.State

type AddrAST = [AddrEx]

data AddrEx = AddrVarDecl AddrDecl
            | AddrFunDef AddrDecl [AddrEx] [AddrIn]
              deriving Show

data AddrIn = AddrAssign AddrDecl AddrIn
            | AddrWrite AddrDecl AddrDecl
            | AddrRead AddrDecl AddrDecl
            | AddrLabel String
            | AddrIf AddrDecl AddrIn AddrIn
            | AddrGoto AddrIn
            | AddrCall AddrDecl AddrDecl [AddrDecl]
            | AddrVCall AddrDecl [AddrDecl]
            | AddrReturn AddrDecl
            | AddrVReturn
            | AddrPrint AddrDecl
            | AddrAdd AddrDecl AddrDecl
            | AddrSub AddrDecl AddrDecl
            | AddrMul AddrDecl AddrDecl
            | AddrDiv AddrDecl AddrDecl                  
            | AddrGT AddrDecl AddrDecl
            | AddrST AddrDecl AddrDecl
            | AddrGE AddrDecl AddrDecl
            | AddrSE AddrDecl AddrDecl
            | AddrEQ AddrDecl AddrDecl
            | AddrNE AddrDecl AddrDecl
            | AddrAddr AddrDecl
            | AddrVar AddrDecl
            | AddrLit Integer
              deriving Show

data AddrDecl = AddrDecl {adname :: String, adkind :: Kind, adty :: SemType, adfp::Int}
                deriving Show

type WithFp = State Int

