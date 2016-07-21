module CFGTypeDef where

import AddrTypeDef
import SemanticTypeDef
import Data.List

type OptAST = [OptEx]

data OptEx = OptFunc Decl [AddrIn] [OptNode]
           | OptVar AddrEx

instance Show OptEx where
    show (OptFunc d args node) = (show d) ++ "\n" ++ (show args) ++ "\n" ++ intercalate "\n\n" (map show node)
    show (OptVar e) = "var " ++ (show e)


data OptNode = OptNode {name :: [NodeName], prev :: [NodeName], inter :: [OptIn], next :: [NodeName]}
             deriving Eq

instance Show OptNode where
    show n = "name = " ++ (show (CFGTypeDef.name n)) ++ "\n" ++ (show (prev n)) ++ "\n\t" ++ intercalate "\n\t" (map show (inter n)) ++ "\n" ++ (show (next n))


data OptIn = OptIn {stmt :: AddrIn, line :: Int}
           deriving (Show,Eq)


data NodeName = Begin
              | End
              | Label String
              | None
              deriving (Show,Eq)
