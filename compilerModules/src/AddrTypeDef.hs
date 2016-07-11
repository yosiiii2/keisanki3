module AddrTypeDef where
    
import SemanticTypeDef
import Control.Monad.State
import Data.List

type AddrAST = [AddrEx]

data AddrEx = AddrVarDeclGlobal Decl
            | AddrFunDef Decl [AddrIn] [AddrIn]

instance Show AddrEx where
    show (AddrVarDeclGlobal d) = "(AddrVarDeclGlobal " ++ (show d) ++ ")"
    show (AddrFunDef fund argd inter) = "(AddrFunDef\n " ++ (show fund) ++ "\n\t[" ++ (intercalate "\n\t " (map show argd)) ++ "]\n\t[" ++ (intercalate "\n\t " (map show inter)) ++ "])\n"

data AddrIn = AddrVarDecl Decl
            | AddrAssign Decl AddrIn
            | AddrWrite Decl Decl
            | AddrRead Decl Decl
            | AddrLabel String
            | AddrIf Decl AddrIn AddrIn
            | AddrGoto AddrIn
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

type WithFp = State (Env,Integer,Integer)

