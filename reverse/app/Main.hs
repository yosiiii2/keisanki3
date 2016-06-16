module Main where

import Text.Parsec
import System.Environment (getArgs)
import ParserFunc
import Reverser
import ObjectCollect

run :: String -> String
run input = case parse program "" input of
              Left err -> show err
              Right val -> show $ hoge  $ removeSugar val

main :: IO()
main = do
    str <- getArgs
    args <- readFile (head (str))
    putStrLn (run args)

             
