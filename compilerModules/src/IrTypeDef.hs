module IrTypeDef where

import SemanticTypeDef
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity

type IrAST = [IrExternal]
    
data IrExternal = VarDecl Decl
                | FunDef Decl [IrExternal] [IrInternal]
                  deriving Show

data IrInternal = IrAssign Decl IrInternal
                | IrWrite Decl Decl
                | IrRead Decl Decl
                | IrLabel String -- ここのStringはLabelのname
                | IrIf Decl IrInternal IrInternal -- ここのInternalはIrLabel
                | IrGoto IrInternal -- ここのInternalはIrLabel
                | IrCall Decl Decl [Decl]
                | IrVCall Decl [Decl]
                | IrReturn Decl
                | IrVReturn
                | IrPrint Decl
                | IrComp [IrExternal] [IrInternal]
                | IrAdd Decl Decl
                | IrSub Decl Decl
                | IrMul Decl Decl
                | IrDiv Decl Decl                  
                | IrGT Decl Decl
                | IrST Decl Decl
                | IrGE Decl Decl
                | IrSE Decl Decl
                | IrEQ Decl Decl
                | IrNE Decl Decl
                | IrAddr Decl
                | IrVar Decl
                | IrLit Integer
                  deriving Show

type WithIr = State (Env,Int,Int)
-- type WithIr = StateT (Env,Int,Int) (Writer [IrExternal]) 


