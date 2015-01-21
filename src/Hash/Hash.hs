-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.

module Hash.Hash where

--import Hash.Language.Commands
import Hash.Language.Exec 
import Hash.Language.Expressions
import Hash.Parsing.HashParser

import Text.Parsec (parse, ParseError)

-- Runs a .hash script
{-
runScript :: FilePath -> IO ()
runScript fp = do
    eitherLtlexpr <- parseTLExprsFromFile fp
    let ltlexpr = case eitherLtlexpr of
	 Left err -> error "Script not formatted correctly"
	 Right xs -> xs
    runHashProgram ctable "." ltlexpr
    -}	 
-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
    cont <- getContents
    putStrLn $ show $ parse readTLExpr "Interactive" cont
    

