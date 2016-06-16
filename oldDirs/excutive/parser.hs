import Text.Parsec
import System.Environment (getArgs)
import ParserFunc as P -- my module

    
run :: String -> String
run input = case parse P.program "" input of
              Left err -> show err
              Right val -> show val

main :: IO()
main = do
    str <- getArgs
    args <- readFile(head (str))
    putStrLn (run args)

