-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. We recommend Parsec for parsing.

module Hash.Parsing.HashParser where

import Hash.Language.Expressions
import Text.ParserCombinators.Parsec
import Control.Monad

readExpr :: Parser Expr
readExpr =  readExprVar <|> readExprStr

readExprVar :: Parser Expr
readExprVar = do
    char '$'
    first <- letter
    rest  <- many alphaNum
    return $ Var (first:rest)
    
readExprStr :: Parser Expr
readExprStr = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ Str x

readComp :: Parser Comp
readComp = try readCompPair <|> readCompSingle
    
readCompSingle :: Parser Comp
readCompSingle = liftM CLI $ readExpr
    
    
readCompPair :: Parser Comp
readCompPair = do
    a <- readExpr
    symb <- choice $ fmap (try.string) $ ["==","/=", ">", ">=",">","<=",""] 
    b <- readExpr
    return $ case symb of
	      "==" ->  CEQ a b
	      "/=" ->  CNE a b
	      ">=" ->  CGE a b
	      ">"  ->  CGT a b
	      "<=" ->  CLE a b
	      "<"  ->  CLT a b