module ParserFunc (program,externalDeclaration)where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import TypeDef
import Lexer

program :: Parser [External]    
program = do
  many externalDeclaration <* eof

externalDeclaration :: Parser External
externalDeclaration =
  try(declaration)
  <|>
  try(functionPrototype)
  <|>
  try(functionDefinition)

declaration :: Parser External
declaration = try(do
  p <- getPosition
  t <- typeSpecifier
  dec <- Lexer.commaSep1 declarator
  Lexer.symbol ";"
  return (Declaration p t dec))

declarator :: Parser Decralator
declarator = try(do
  p <- getPosition                 
  Lexer.symbol "*"
  d <- directDeclarator
  return (PointDec p d))
  <|>try(do
  d <- directDeclarator
  p <- getPosition                   
  return (Dec p d))

directDeclarator :: Parser DDec
directDeclarator = try(do
  p <- getPosition
  i <- Lexer.identifier
  Lexer.symbol "["
  c <- Lexer.natural
  Lexer.symbol "]"
  return (ArrayDec p i c))
  <|>try(do
  p <- getPosition                 
  i <- Lexer.identifier
  return (DirectDec p i))

functionPrototype :: Parser External
functionPrototype = try(do
  p <- getPosition
  t <- typeSpecifier
  fd <- functionDecralator
  Lexer.symbol ";"
  return (FuncProto p t fd))

functionDecralator :: Parser FunctionDec
functionDecralator = try(do
  p <- getPosition
  i <- Lexer.identifier
  Lexer.symbol "("
  pl <- Lexer.commaSep parameterDeclaration
  Lexer.symbol ")"
  return (FuncDec p i pl))
  <|>try(do
  p <- getPosition                 
  Lexer.symbol "*"
  i <- Lexer.identifier
  Lexer.symbol "("
  pl <- Lexer.commaSep parameterDeclaration
  Lexer.symbol ")"
  return (PointFuncDec p i pl))

functionDefinition :: Parser External
functionDefinition = try(do
  p1 <- getPosition
  t <- typeSpecifier
  f <- functionDecralator
  Lexer.symbol "{"
  p2 <- getPosition
  dl <- many declaration
  cs <- many statement
  Lexer.symbol "}"
  return (FunctionDef p1 t f (CompStmt p2 dl cs)))
                         
parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = try(do
  p <- getPosition
  t <- typeSpecifier
  Lexer.symbol "*"
  i <- Lexer.identifier
  return (PointParaDec p t i))
  <|>try(do
  p <- getPosition
  t <- typeSpecifier
  i <- Lexer.identifier
  return (ParaDec p t i))

         
typeSpecifier :: Parser Type
typeSpecifier = try(do
  p <- getPosition
  Lexer.reserved "int"
  return (Int p))
  <|>try(do
  p <- getPosition
  Lexer.reserved "void"
  return (Void p))

         
statement :: Parser Stmt
statement = do
  p <- getPosition
  Lexer.semi  >> return (SemiOnly p)
  <|>try(do
    p <- getPosition
    Lexer.reserved "if"
    Lexer.symbol "("
    cond <- mExpression
    Lexer.symbol ")"
    st <- statement
    return (If p cond st))
  <|>try(do
    p <- getPosition
    Lexer.reserved "if"
    Lexer.symbol "("
    cond <- mExpression
    Lexer.symbol ")"
    st1 <- statement
    Lexer.reserved "else"
    st2 <- statement
    return (IfElse p cond st1 st2))
  <|>try(do
    p <- getPosition
    Lexer.reserved "while"
    Lexer.symbol "("
    cond <- mExpression
    Lexer.symbol ")"
    st <- statement
    return (While p cond st))
  <|>try(do
    p <- getPosition
    Lexer.reserved "for"
    Lexer.symbol "("
    f <- option (Const p 1) mExpression
    Lexer.symbol ";"
    s <- option (Const p 1) mExpression
    Lexer.symbol ";"
    t <- option (Const p 1) mExpression
    Lexer.symbol ")"
    st <- statement
    return (For p f s t st))
  <|>try(do
    p <- getPosition
    Lexer.reserved "return"
    e <- mExpression
    Lexer.semi
    return (Return p e))
  <|>try(do
    p <- getPosition
    e <- mExpression
    Lexer.semi
    return (Statement p e))
  <|>try(do
    p <- getPosition
    Lexer.symbol "{"
    dl <- many declaration
    cs <- many statement
    Lexer.symbol "}"
    return (CompStmt p dl cs))

mExpression :: Parser Exp
mExpression = try(do
               p <- getPosition                      
               e <- Lexer.commaSep1 expression
               return (ManyExp p e))
              <|>
              expression
         
expression :: Parser Exp
expression = buildExpressionParser table factor
            where
               table = [[unaop "-" Neg, unaop "&" Address, unaop "*" Pointer],
                        [binop "*" Mul AssocLeft, binop "/" Div AssocLeft],
                        [binop "+" Add AssocLeft, binop "-" Sub AssocLeft],
                        [binop "<" Small AssocLeft, binop ">" Large AssocLeft,
                         binop "<=" SmallEq AssocLeft, binop ">=" LargeEq AssocLeft],
                        [binop "==" Equal AssocLeft, binop "!=" NotEqual AssocLeft],
                        [binop "&&" And AssocLeft],
                        [binop "||" Or AssocLeft],
                        [binop "=" Assign AssocLeft]
                       ]
                   where binop op ret assoc = Infix (do{p<-getPosition;Lexer.reservedOp op; (return (ret p))}) assoc
                         unaop op ret = Prefix (do{p<-getPosition;Lexer.reservedOp op; (return (ret p))})
               factor ::Parser Exp
               factor =try(do
                   p <- getPosition
                   Lexer.symbol "("
                   e <- mExpression
                   Lexer.symbol ")"
                   return (Paren p e))
                 <|>try(do
                   p <- getPosition
                   i <- Lexer.identifier
                   Lexer.symbol "("
                   e <- many expression
                   Lexer.symbol ")"
                   return (Func p (Id p i) e))
                 <|>try(do
                   p <- getPosition
                   i <- Lexer.identifier
                   Lexer.symbol "["
                   e <- mExpression
                   Lexer.symbol "]"
                   return (Array p (Id p i) e))
                 <|>try(do
                   p <- getPosition
                   n <- Lexer.natural
                   return (Const p n))
                 <|>try(do
                   p <- getPosition
                   i <- Lexer.identifier
                   return (Id p i))

