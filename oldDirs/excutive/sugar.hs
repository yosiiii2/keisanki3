import Text.Parsec
import System.Environment (getArgs)
import ParserFunc as P -- my module
import RemoveSugar -- my module

-- import System

run :: String -> String
run input = case parse program "Program" input of
              Left err -> show err
              Right val -> show $ removeSugar val

main :: IO()
main = do
    str <- getArgs
    args <- readFile(head (str))
    putStrLn (run args)

             
