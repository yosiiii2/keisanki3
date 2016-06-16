module Lexer where
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token as Token

                     
smallCDef :: LanguageDef ()
smallCDef = emptyDef{
              commentStart = "/*"
              ,commentEnd = "*/"
              ,commentLine = "//"
              ,nestedComments = False
              ,identStart = letter
              ,identLetter = alphaNum
              ,opStart = oneOf "+-*/<=>&!|"
              ,reservedOpNames = ["+","-","*","/","<","<=",">",">=","==","!=","&",
                                   "&&","||","="]
              ,reservedNames = ["return", "if", "else", "while", "for", "int", "void"]
              ,caseSensitive = True
              }

lexer :: Token.TokenParser()
lexer = Token.makeTokenParser smallCDef

        
whiteSpace= Token.whiteSpace lexer
lexeme    = Token.lexeme lexer
symbol    = Token.symbol lexer
natural   = Token.natural lexer
parens    = Token.parens lexer
semi      = Token.semi lexer
identifier= Token.identifier lexer
reserved  = Token.reserved lexer
reservedOp= Token.reservedOp lexer
commaSep1 = Token.commaSep1 lexer
braces = Token.braces lexer
commaSep = Token.commaSep lexer

