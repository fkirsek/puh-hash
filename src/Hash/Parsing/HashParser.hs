-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. We recommend Parsec for parsing.

module Hash.Parsing.HashParser where

import Hash.Language.Expressions
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative ( (<$>), (<*>), (<*) )

{-
endOfCommand :: Parser Char
endOfCommand = newline <|> eof
-}


readExprVar :: Parser Expr
readExprVar = do
    char '$'
    first <- letter
    rest  <- many alphaNum
    return $ Var (first:rest)

-- if you want a string the way haskell would understand it... I think!
readEncloseString :: Parser String
readEncloseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return x
 
readEscapedChar :: Parser String
readEscapedChar = do
    char '\\'
    a <- anyChar
    return [a]
 
readExprStr :: Parser Expr
readExprStr = do	 
    x <- many1 (readEscapedChar <|> readEncloseString <|> many1 (noneOf "\" \\\n\t") )
    return $ Str $ concat x
    
readExpr :: Parser Expr
readExpr =  try readExprVar <|> readExprStr

    
readComp :: Parser Comp
readComp = try readCompPair <|> readCompSingle
    
readCompSingle :: Parser Comp
readCompSingle = liftM CLI $ readExpr
    
    
readCompPair :: Parser Comp
readCompPair = do
    a <- readExpr
    spaces
    symb <- choice $ fmap (try.string) $ ["==","/=", ">", ">=",">","<=",""] 
    spaces
    b <- readExpr
    return $ case symb of
	      "==" ->  CEQ a b
	      "/=" ->  CNE a b
	      ">=" ->  CGE a b
	      ">"  ->  CGT a b
	      "<=" ->  CLE a b
	      "<"  ->  CLT a b
	      
readPredComp :: Parser Pred
readPredComp = liftM Pred $ readComp

readPred :: Parser Pred
readPred = try readPredParens <|> try readPredBin <|> try readPredNot <|> readPredComp

readPredNot :: Parser Pred
readPredNot =  char '!' >> (liftM Not $ readPred)

readPredBin :: Parser Pred
readPredBin = do
    a <- try readPredParens <|> readPredComp
    spaces
    symb <- choice $ fmap (try.string) $ ["&&", "||"]
    spaces
    b <- try readPredParens <|> readPredComp
    return $ case symb of
		  "&&" -> And a b
		  "||" -> Or  a b

readPredParens :: Parser Pred
readPredParens = do
  char '(' 
  spaces
  a <- readPred
  char ')'
  return $ Parens a
  
readCmdAssign :: Parser Cmd 
readCmdAssign = do
    spaces
    var1 <- readExprVar
    spaces
    char '='
    spaces
    val1 <- readExpr
    return $ Assign { var = var1, val = val1}

readCmdName :: Parser String
readCmdName = (:) <$> letter <*> many alphaNum

-- a true command must be written in one line
readCmdCmd :: Parser Cmd 
readCmdCmd = do
   name1 <-  readCmdName
   (many $ char ' ' <|> char '\t')
   args1 <- sepBy readExpr (many $ char ' ' <|> char '\t') -- this still doesn't handle the redirection!
   optional $ char '\n'
   return $ Cmd {
       name = Str name1
     , args = args1
     , inDir = Nothing
     , outDir = Nothing
     , append = False
   }