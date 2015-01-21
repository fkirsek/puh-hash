-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.

module Hash.Hash where

--import Hash.Language.Commands
import Hash.Language.Exec 
import Hash.Language.Expressions
import Hash.Parsing.HashParser
import Hash.Language.Commands

import Text.Parsec (parse, ParseError)

import qualified Data.Map as M
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript fp = do
    eitherLtlexpr <- parseTLExprsFromFile fp
    let ltlexpr = case eitherLtlexpr of
	 Left err -> error "Script not formatted correctly"
	 Right xs -> xs
    runHashProgram commands (Left ".") ltlexpr
    return ()
    
-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
  keepOnParsing $ ScriptState { output = [], wd = [], vartable = M.empty}
  return ()
	   --putStrLn $ show $ parse readTLExpr "Interactive" cont

parseOneLine :: ScriptState -> IO ScriptState
parseOneLine sstate = do
  cont <- getLine
  let parsed = parse readTLExpr "Interactive" cont
  --putStrLn $ show $ parsed
  case parsed of
	 Left err -> do
	   putStrLn "Error: incorrect syntax"
	   return sstate
	 Right a  -> do
	   newstate <- runHashProgram commands (Right sstate) [a] 
	   return newstate
	   
keepOnParsing :: ScriptState -> IO ScriptState
keepOnParsing sstate = do
  newstate <- parseOneLine sstate
  if(wd newstate == ";quit") 
	      then return newstate
	      else keepOnParsing newstate