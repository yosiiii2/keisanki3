module SemanticTypeDef where

import Text.Parsec.Pos
import TypeDef
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import Data.List

-- declaration of "Decl" structure
data Decl = Decl {name :: String, kind :: Kind, t :: SemType}
          | AddrDecl {name :: String, kind :: Kind, t :: SemType, decfp :: Integer}
            deriving Show

instance Eq Decl where
    (Decl s1 k1 t1) == (Decl s2 k2 t2) = (s1==s2) && (k1==k2) && (t1==t2)
    (AddrDecl s1 k1 t1 f1) == (AddrDecl s2 k2 t2 f2) = (s1==s2) && (k1==k2) && (t1==t2) && (f1==f2)
    _ == _ = False


data Kind = Var
          | Parm
          | Fun
          | Proto
            deriving (Show , Eq)

data SemType = SInt
             | SVoid
             | SPointer {ptype :: SemType}
             | SArray {atype :: SemType, n :: ArrayNum}
             | SFunc {ftype :: SemType, fargs :: [SemType]}
             | STemp
                        
instance Show SemType where
    show SInt = "int"
    show SVoid = "void"
    show (SPointer typ) = ("*" ++ (show typ))
    show (SArray typ num) = (show num) ++ " array of " ++ (show typ)
    show (SFunc typ tys) = (show typ) ++ " function" ++ " with argument " ++ (intercalate "," (map show tys))
    show (STemp) = "Temp"
                   
instance Eq SemType where
    SInt == SInt = True -- SInt同士は等しい
    SVoid == SVoid = True -- SVoid同士も等しい
    SPointer t1 == SPointer t2 = (t1 == t2) -- SPointer同士は、中の型が等しいなら等しい
    SPointer t1 == SArray t2 _ = (t1 == t2) -- SPointerとSArrayは、中の型が等しいなら等しい
    SArray t2 _ == SPointer t1 = (t1 == t2) -- SPointerとSArrayは、中の型が等しいなら等しい
    SArray t1 _ == SArray t2 _ = (t1 == t2) -- SArray同士は、中の型が等しいなら等しい
    SFunc t1 a1 == SFunc t2 a2 = (t1 == t2) && (a1 == a2) -- SFunc同士は、戻り値の型が等しく、且つ引数の型が等Sしいなら等しい
    (SFunc t1 _) == t2 = (t1 == t2) -- SFuncと他の型は、SFuncの戻り値の型ともう一方の型が等しいなら等しい
    t2 == (SFunc t1 _)  = (t1 == t2) -- SFuncと他の型は、SFuncの戻り値の型ともう一方の型が等しいなら等しい
    STemp == STemp = True
    _ == _ = False -- それ以外は全部False


-- declaration of AST including object information
type SAST = [SExternal]

data SExternal = SFuncProto SourcePos Decl [Decl]
               | SFunctionDef SourcePos Decl [Decl] SStmt
               | SDeclaration SourcePos [Decl]
                 deriving Show
                          
data SStmt = SStatement SourcePos BothExp
           | SSemiOnly SourcePos
           | SCompStmt SourcePos [SExternal] [SStmt] 
           | SIfElse SourcePos BothExp SStmt SStmt 
           | SWhile SourcePos BothExp SStmt
           | SReturn SourcePos BothExp
           | SVReturn SourcePos
             deriving Show

data SExp = SAssign {ty :: SemType, exp1::SExp, exp2::SExp}
          | SOr {ty :: SemType, exp1::SExp, exp2::SExp}
          | SAnd {ty :: SemType, exp1::SExp, exp2::SExp}
          | SEqual {ty :: SemType, exp1::SExp, exp2::SExp}
          | SNotEqual {ty :: SemType, exp1::SExp, exp2::SExp}
          | SSmall {ty :: SemType, exp1::SExp, exp2::SExp}
          | SLarge {ty :: SemType, exp1::SExp, exp2::SExp}
          | SSmallEq {ty :: SemType, exp1::SExp, exp2::SExp}
          | SLargeEq {ty :: SemType, exp1::SExp, exp2::SExp} 
          | SAdd {ty :: SemType, exp1::SExp, exp2::SExp} 
          | SSub {ty :: SemType, exp1::SExp, exp2::SExp} 
          | SMul {ty :: SemType, exp1::SExp, exp2::SExp} 
          | SDiv {ty :: SemType, exp1::SExp, exp2::SExp} 
          | SAddress {ty :: SemType, exp::SExp} 
          | SPointerExp {ty :: SemType, exp::SExp} 
          | SFuncExp {ty :: SemType, theNameOfFunction :: String, theArgsOfFunction :: [SExp]}
          | SId {ty :: SemType, theNameOfIdentifier :: String}
          | SConst {ty :: SemType, theNumberOfConstant :: Integer}
          | SManyExp {ty :: SemType , theInsideOfTheSME :: [SExp]}
            deriving Show



data BothExp = Expression Exp
             | SemanticExpression SExp
               deriving Show


    
-- declaration of AST with environment (This is AST and list of list of Decl)
type Env = [M.Map String Decl]
-- type WithEnv = StateT Env (Except ObjectErr)
type WithEnv = WithEnvT (Writer [String])
type WithEnvT m = StateT Env (ExceptT ObjectErr m)
type ObjectErr = String
