-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.

module Hash.Hash where

--import Hash.Language.Commands
import Hash.Language.Exec 
import Hash.Language.Expressions
import Hash.Parsing.HashParser
import Hash.Language.Commands

-- exceptions
import Control.Exception

import Text.Parsec (parse, ParseError)

import qualified Data.Map as M

catchParseExp :: SomeException -> IO [TLExpr]
catchParseExp e = putStrLn "Invalid script syntax" >> return []

-- Runs a .hash script
runScript :: FilePath -> [String] -> IO ()
runScript fp args = do
    ltlexpr <- catch (parseTLExprsFromFile fp args) $ catchParseExp
    catch (runHashProgram commands (Left ".") ltlexpr) $ catchAny (ScriptState {output="", wd="", vartable = M.empty}) "Invalid usage of functions"
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
	   putStrLn "Error: invalid syntax"
	   return sstate
	 Right a  -> do
	   newstate <- catch (runHashProgram commands (Right sstate) [a] ) $ catchAny sstate "Invalid usage"
	   return newstate
	   
catchAny :: ScriptState -> String -> SomeException -> IO ScriptState
catchAny sstate mes _= putStrLn mes >> return sstate
 
	   
keepOnParsing :: ScriptState -> IO ScriptState
keepOnParsing sstate = do
  newstate <- parseOneLine sstate
  if(wd newstate == ";quit") 
	      then return newstate
	      else keepOnParsing newstate