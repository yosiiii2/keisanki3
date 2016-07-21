module Main where

import System.IO
import Text.Parsec
import System.Environment (getArgs)
import ParserFunc -- my module
import ObjectCollect -- my module
import RemoveSugar -- my module
import Ir -- my module
import Address -- my module
import Gen
import GenTypeDef
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.List

run :: String -> Either String String
run input = case parse program "Program" input of
              Left err -> (Left (show err))
              Right val ->
                  let hoge = runWriter (runExceptT (evalStateT (objCol $ removeSugar val) []))
                  in case (fst hoge) of
                       Left err2 -> (Left (show err2))
                       Right val2 -> if(null $ snd hoge)
                                     then (Right (showMips $ generate $ evalState (makeAddr (evalState (makeIr val2) ([],0,0))) ([],0,0)))
                                     else (Right ((intercalate "\n" (map show (snd hoge)))
                                           ++ "\n"
                                          ++ (intercalate "\n" (map show $ generate $ evalState (makeAddr (evalState (makeIr val2) ([],0,0))) ([],0,0)))))

main :: IO()
main = do
    str <- getArgs
    args <- readFile(head (str))
    case run args of
      Left err -> hPutStr stderr (err ++ "\n\n")
      Right out -> putStr out
