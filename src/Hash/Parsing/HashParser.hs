-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. We recommend Parsec for parsing.

module Hash.Parsing.HashParser where

import Hash.Language.Expressions
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative ( (<$>), (<*>), (<*) )
import Data.List
{-
endOfCommand :: Parser Char
endOfCommand = newline <|> eof
-}

skipwot :: Parser ()
skipwot = (skipMany $ char ' ' <|> char '\t')


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
    x <- many1 (readEscapedChar <|> readEncloseString <|> many1 (noneOf "\" \\\n\t;") )
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
    symb <- choice $ fmap (try.string) $ ["==","/=", ">", ">=",">","<=",""] 
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
    symb <- choice $ fmap (try.string) $ ["&&", "||"]
    b <- try readPredParens <|> readPredComp
    return $ case symb of
		  "&&" -> And a b
		  "||" -> Or  a b

readPredParens :: Parser Pred
readPredParens = do
  char '(' 
  a <- readPred
  char ')'
  return $ Parens a
  
readCmdAssign :: Parser Cmd 
readCmdAssign = do
    skipwot
    var1 <- readExprVar
    char '='
    val1 <- readExpr
    skipwot
    return $ Assign { var = var1, val = val1}

{-
readCmdName :: Parser String
readCmdName = (:) <$> letter <*> many alphaNum
-}

-- a true command must be written in one line, with its arguments separated by spaces
readCmdCmd :: Parser Cmd 
readCmdCmd = do
    skipwot
    name1 <-  readExpr
    skipwot
    args1 <- sepBy readExpr (many $ char ' ' <|> tab)
    skipwot
    let (args2, inDir1, outDir1, append1) = handleRedirects args1
    return $ Cmd {
       name = name1
     , args = args2
     , inDir = inDir1
     , outDir = outDir1
     , append = append1
   }
   
-- handleing redirects manually, not the smartest buuuut hey
handleRedirects :: [Expr] -> ( [Expr], Maybe Expr, Maybe Expr, Bool)
handleRedirects args = outRedir $ inRedir args
    

filterOutIndexes :: [a] -> Int -> Int -> [a]
filterOutIndexes list arg1 arg2 = map snd $ filter (\x -> fst x /= arg1 && fst x /= arg2) $ zip [0..] list

inRedir :: [Expr] -> (Maybe Expr, [Expr])
inRedir args = case findIndices (==Str "<") args of
		    [] -> (Nothing, args)
		    a  -> (Just $ args !! (last a + 1), filterOutIndexes args (last a) (last a +1) )
		   
outRedir :: (Maybe Expr, [Expr]) -> ( [Expr], Maybe Expr, Maybe Expr, Bool)
outRedir (inRedir, args) = case findIndices (== Str ">") args of
			      [] -> case findIndices (== Str ">>") args of
				      [] -> (args, Nothing, inRedir, False)
				      a  -> (filterOutIndexes args (last a) (last a + 1), Just $ args !! (last a + 1), inRedir, True)
			      a  -> (filterOutIndexes args (last a) (last a + 1), Just $ args !! (last a + 1), inRedir, False)
			      
readCmd = try readCmdAssign <|> readCmdCmd
			      
-- parses out comments 
readComment :: Parser ()
readComment =  skipwot >> char '#' >> many (noneOf "\n") >> newline >> return ()
   
-- parsing a conditional
readOnlyIfPart :: Parser (Pred, [Cmd])
readOnlyIfPart = do
    skipwot
    string ";if"
    skipwot
    pred <- readPred
    readTLExpr
    newline
    string ";then" 
    coms <- endBy (try readCmd) newline
    return (pred, coms)
    
readIf :: Parser Conditional
readIf = do
    (pred, coms) <- readOnlyIfPart
    skipwot
    string ";fi"
    return $ If{ cond = pred, cthen = coms }

readIfThen :: Parser Conditional
readIfThen = do
    (pred, coms) <- readOnlyIfPart	
    skipwot
    string ";else"
    skipwot
    elseComs <- endBy (try readCmd) newline
    skipwot
    string ";fi"
    return $ IfElse { cond = pred, cthen = coms, celse = elseComs}
    
-- else and fi in conditionals must begin with a ;, ie. ;else and ;fi
readConditional :: Parser Conditional
readConditional = try readIfThen <|> readIf

readTLExpr :: Parser TLExpr
readTLExpr = (TLCnd <$> try readConditional) <|> (TLCmd <$> try readCmd)

parseExprFromFile fp = parseFromFile (sepBy readExpr (many $ char ' ' <|> char '\t')) fp

readManyTLExpr :: Parser [TLExpr]
readManyTLExpr = many readTLExpr

parseTLExprsFromFile fp = parseFromFile readManyTLExpr fp