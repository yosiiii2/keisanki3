module ParserFunc (program,externalDeclaration)where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import TypeDef
import Lexer

-- ファイルの終端まで読み込んで、Externalのリストとして受け取る
program :: Parser [External]    
program = do
  many externalDeclaration <* eof

-- Externalはプロトタイプ宣言か関数定義か変数宣言のいずれかとして処理する
externalDeclaration :: Parser External
externalDeclaration =
  try(functionPrototype)
  <|>
  try(functionDefinition)
  <|>
  try(declaration)
 
-- 変数宣言は、
declaration :: Parser External
declaration = try(do
  p <- getPosition -- ポジションを取り、
  t <- typeSpecifier -- 型を取り、
  dec <- Lexer.commaSep1 (declarator t) -- ,で区切られた各変数を取ってリストとし、
  Lexer.symbol ";" -- 最後に;がくる
  return (Declaration p dec)) -- それらをDeclarationにくるんで返す

 -- declaratorは、変数宣言で最初に書かれる型を引数に取って、
declarator :: Type -> Parser Declarator
declarator t = try(do
  p <- getPosition 
  Lexer.symbol "*"
  s <- Lexer.identifier
  Lexer.symbol "["
  c <- Lexer.natural
  Lexer.symbol "]"  
  return (Declarator p t s True c)) -- "*"があって、[]があったらポインタの配列として、PointOrがTrueかつArrayNumが[]の中身のDeclaratorとして返す
  <|>try(do
  p <- getPosition
  s <- Lexer.identifier
  Lexer.symbol "["
  c <- Lexer.natural
  Lexer.symbol "]"  
  return (Declarator p t s False c))  -- "*"がなくて、[]があったらただの配列として、PointOrがFalseかつArrayNumが[]の中身のDeclaratorとして返す
  <|>try(do
  p <- getPosition
  Lexer.symbol "*"
  s <- Lexer.identifier
  return (Declarator p t s True (-1))) -- "*"があって、[]がなかったらただのポインタとして、PointOrがTrueかつArrayNumが-1のDeclaratorとして返す
  <|>try(do
  p <- getPosition
  s <- Lexer.identifier
  return (Declarator p t s False (-1))) -- "*"も[]もなかったらただの宣言として、PointOrがFalseかつArrayNumが-1のDeclaratorとして返す

 -- パラメータの宣言の時は変数宣言とは違い、配列を持たないので、それはパースしないようにする
paramDeclarator :: Parser Declarator
paramDeclarator = try(do
  p <- getPosition
  t <- typeSpecifier     
  Lexer.symbol "*"
  s <- Lexer.identifier
  return (Declarator p t s True (-1)))
  <|>try(do
  p <- getPosition
  t <- typeSpecifier     
  s <- Lexer.identifier
  return (Declarator p t s False (-1)))

-- プロトタイプ宣言は
functionPrototype :: Parser External
functionPrototype = try(do
  p <- getPosition
  t <- typeSpecifier
  Lexer.symbol "*"
  ident <- Lexer.identifier
  Lexer.symbol "("  
  fd <- Lexer.commaSep paramDeclarator
  Lexer.symbol ")"
  Lexer.symbol ";"  
  return (FuncProto p t True ident fd)) -- "*"があったらPointOrをTrueにして、あとは各々入れる
  <|>try(do
  p <- getPosition
  t <- typeSpecifier
  ident <- Lexer.identifier
  Lexer.symbol "("  
  fd <- Lexer.commaSep paramDeclarator
  Lexer.symbol ")"
  Lexer.symbol ";"  
  return (FuncProto p t False ident fd)) -- "*"がなかったらPointOrをFalseにして、あとは各々入れる

-- 関数宣言もプロトタイプ宣言と同様にする
-- ただし、引数の後ろにstatementではなく複文がくることに気をつける
functionDefinition :: Parser External
functionDefinition = try(do
  p1 <- getPosition
  t <- typeSpecifier
  Lexer.symbol "*"
  ident <- Lexer.identifier
  Lexer.symbol "("  
  d <- Lexer.commaSep paramDeclarator
  Lexer.symbol ")"
  Lexer.symbol "{"
  p2 <- getPosition
  dl <- many declaration
  cs <- many statement
  Lexer.symbol "}"
  return (FunctionDef p1 t True ident d (CompStmt p2 dl cs)))
  <|>try(do
  p1 <- getPosition
  t <- typeSpecifier
  ident <- Lexer.identifier
  Lexer.symbol "("  
  d <- Lexer.commaSep paramDeclarator
  Lexer.symbol ")"  
  Lexer.symbol "{"
  p2 <- getPosition
  dl <- many declaration
  cs <- many statement
  Lexer.symbol "}"
  return (FunctionDef p1 t False ident d (CompStmt p2 dl cs)))

-- 型をパースする         
typeSpecifier :: Parser Type
typeSpecifier = try(do
  p <- getPosition
  Lexer.reserved "int"
  return (Int p))
  <|>try(do
  p <- getPosition
  Lexer.reserved "void"
  return (Void p))

-- 各種statementをパースする。
-- 基本的にpositionを取って、expressionが入るところをmExpressionでパース
-- あとは()とかを、あるべき場所でパースしている
-- 詳細は割愛
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
    st1 <- statement
    Lexer.reserved "else"
    st2 <- statement
    return (IfElse p cond st1 st2))
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
    Lexer.semi
    return (VReturn p))
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

-- expressionとしてとるか、","で区切られたexpressionをManyExpressionとして固めて返す
mExpression :: Parser Exp
mExpression = try(do
               p <- getPosition
               e1 <- expression
               Lexer.symbol ","
               e <- Lexer.commaSep1 expression
               return (ManyExp p (e1:e)))
              <|>
               expression

-- expressionの定義
-- 各種オペランドの定義と優先順位、終端記号を定義するとうまいことパースしてくれる.
-- ただし、単項オペランドの連続を読み込めないので、まとめて読み込み、別でパースする
-- 変数呼び出し,関数呼び出し,定数,()を終端記号として扱っている。
expression :: Parser Exp
expression = buildExpressionParser table factor
            where
              table = [
               [binop "*" Mul AssocLeft, binop "/" Div AssocLeft],
               [binop "+" Add AssocLeft, binop "-" Sub AssocLeft],
               [binop "<" Small AssocLeft, binop ">" Large AssocLeft,
                binop "<=" SmallEq AssocLeft, binop ">=" LargeEq AssocLeft],
               [binop "==" Equal AssocLeft, binop "!=" NotEqual AssocLeft],
               [binop "&&" And AssocLeft],
               [binop "||" Or AssocLeft],
               [binop "=" Assign AssocRight]]
              
              binop op ret assoc = Infix (do{p<-getPosition;Lexer.reservedOp op; (return (ret p))}) assoc
                           
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
                   e <- Lexer.commaSep expression
                   Lexer.symbol ")"
                   return (Func p i e))
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
                 <|>try(do
                   p <- getPosition
                   Lexer.symbol "*"
                   e <- factor
                   return (Pointer p e))
                 <|>try(do
                   p <- getPosition
                   Lexer.symbol "-"
                   e <- factor
                   return (Neg p e))
                 <|>try(do
                   p <- getPosition
                   Lexer.symbol "&"
                   e <- factor
                   return (Address p e))




