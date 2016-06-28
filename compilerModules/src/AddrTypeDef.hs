module AddrTypeDef where

import SemanticTypeDef

data AddrEx = AddrVarDecl Decl
            | AddrFunDef Decl [AddrIn]
              deriving Show

data AddrIn = AddrAssign Decl AddrIn
            | AddrWrite Decl Decl
            | AddrRead Decl Decl
            | AddrLabel String -- ここのStringはLabelのname
            | AddrIf Decl AddrIn AddrIn -- ここのInternalはIrLabel
            | AddrGoto AddrIn -- ここのInternalはIrLabel
            | AddrCall Decl Decl [Decl]
            | AddrVCall Decl [Decl]
            | AddrReturn Decl
            | AddrVReturn
            | AddrPrint Decl
            | AddrAdd Decl Decl
            | AddrSub Decl Decl
            | AddrMul Decl Decl
            | AddrDiv Decl Decl                  
            | AddrGT Decl Decl
            | AddrST Decl Decl
            | AddrGE Decl Decl
            | AddrSE Decl Decl
            | AddrEQ Decl Decl
            | AddrNE Decl Decl
            | AddrAddr Decl
            | AddrVar Decl
            | AddrLit Integer
              deriving Show

data AddrDecl = Decl {name :: String, kind :: Kind, t :: SemType, fp::Int}
                deriving Show


