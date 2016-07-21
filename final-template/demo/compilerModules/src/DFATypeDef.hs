module DFATypeDef where

import CFGTypeDef
import qualified Control.Monad.State as St
import qualified Control.Monad.Reader as Re
import qualified Data.Set as S
import qualified Data.Map as M

data Which = In | Out
           deriving (Show, Eq, Ord)

type Def  = (String,Int)
type Defs = S.Set Def
type Dict = M.Map (Int,Which) Defs
type WithCFG = St.StateT Dict (Re.Reader [OptNode])
type WithConst = St.StateT [OptNode] (Re.Reader Dict)
type WithConstT m = Re.ReaderT Dict (St.StateT [OptNode] m)
