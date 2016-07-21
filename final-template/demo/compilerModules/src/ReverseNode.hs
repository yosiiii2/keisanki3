module ReverseNode where

import AddrTypeDef
import CFGTypeDef


reverseCFG :: OptAST -> AddrAST
reverseCFG [] = []
reverseCFG (x:xs) =
    let now = case x of
                (OptVar ex) -> ex
                (OptFunc d args node) -> (AddrFunDef d args (reverseNode node))
    in now : (reverseCFG xs)

reverseNode ::[OptNode] -> [AddrIn]
reverseNode [] = []
reverseNode ((OptNode itsName _ internal _):xs) = ((reverseName itsName) ++ (map stmt internal)) ++ (reverseNode xs)


reverseName :: [NodeName] -> [AddrIn]
reverseName [] = []
reverseName (x:xs) =
    let now = case x of
                (Label str) -> [(AddrLabel str)]
                _ -> []
    in (now ++ reverseName xs)
