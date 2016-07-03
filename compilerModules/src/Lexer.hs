module Lexer where
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token as Token

-- 言語の定義                     
smallCDef :: LanguageDef ()
smallCDef = emptyDef{
              commentStart = "/*"
              ,commentEnd = "*/"
              ,commentLine = "//"
              ,nestedComments = False
              ,identStart = letter
              ,identLetter = alphaNum <|> (Text.Parsec.char '_')
              ,opStart = oneOf "+-*/<=>&!|"
              ,reservedOpNames = ["+","-","*","/","<","<=",">",">=","==","!=","&",
                                   "&&","||","="]
              ,reservedNames = ["return", "if", "else", "while", "for", "int", "void"]
              ,caseSensitive = True
              }

-- 言語の定義に基づいたlexerの定義
lexer :: Token.TokenParser()
lexer = Token.makeTokenParser smallCDef


-- 各シンボルの定義

symbol     = Token.symbol lexer
natural    = Token.natural lexer
semi       = Token.semi lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
commaSep1  = Token.commaSep1 lexer
commaSep   = Token.commaSep lexer
char       = Token.charLiteral lexer
