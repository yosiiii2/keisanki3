module Main where

import Text.Parsec
import System.Environment (getArgs)
import ParserFunc -- my module
import ObjectCollect -- my module
import RemoveSugar -- my module
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.List

run :: String -> String
run input = case parse program "Program" input of
              Left err -> show err
              Right val -> let hoge = runWriter (runExceptT (runStateT (objCol $ removeSugar val) []))
                           in case (fst hoge) of
                             Left err2 -> show err2
                             Right val2 -> if(null $ snd hoge)
                                           then show val2
                                           else ((intercalate "\n" (map show (snd hoge))) ++ "\n" ++  (show val2))


main :: IO()
main = do
    str <- getArgs
    args <- readFile(head (str))
    putStrLn (run args)

             
