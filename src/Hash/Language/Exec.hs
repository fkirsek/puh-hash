module Hash.Language.Exec where

import Hash.Language.Expressions
import Hash.Parsing.HashParser

import qualified Data.Map as M
import Data.Maybe
import System.IO
import Control.Monad
import Text.Parsec.String
import Hash.Language.Expressions
import Text.ParserCombinators.Parsec
import Control.Applicative ( (<$>), (<*>), (<*) )
import Data.List

-- A model of a command which is waiting for arguments and a state to run
type Command  = [String] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output   :: String
                               , wd       :: FilePath
                               , vartable :: VarTable
                               } deriving Show

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command’s execution.
{-
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr]
                  -> IO ScriptState
                  -}
                  {-
-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
-}
--The rest of the module should consist of similar functions, calling each
--other so that each expression is parsed by a lower-level function and the
--result can be used in a higher-level function. The Command table and state
--are passed around as necessary to evaluate commands, assignments and
--variable substitution. A better way to pass around variables would be to
--use the State monad or even the StateT monad transformer to wrap IO into it.

-- helper function that transforms an Expr into it's value, using a VarTable vartable
evalExpr :: Expr -> VarTable -> String
evalExpr expr vartable = 
    case expr of
	 Var v -> case M.lookup v vartable of
		      Nothing -> ""
		      Just val -> val 
	 Str s -> s

evalComp :: Comp -> VarTable -> Bool
evalComp comp vartable = 
    case comp of
	 CEQ a b -> (val a == val b)
	 CNE a b -> (val a /= val b)
	 CGE a b -> (val a >= val b)
	 CGT a b -> (val a >= val b)
	 CLE a b -> (val a <= val b)
	 CLT a b -> (val a <  val b)
	 CLI a   -> (val a == "")
    where val a = evalExpr a vartable
	  
evalPred :: Pred -> VarTable -> Bool
evalPred pred vartable = 
    case pred of
	 Pred a   -> evalComp a vartable
	 Not a    -> val a
	 And a b  -> val a && val b
	 Or  a b  -> val a || val b
	 Parens a -> val a
    where val pred = evalPred pred vartable
	  
-- function that evaluates assingment command
evalCmdAssign :: Cmd -> ScriptState -> ScriptState
evalCmdAssign asgn sstate =
    sstate {vartable = newvartable}
      where 
	Var key  	   = var asgn
	Str value	   = val asgn
	table  	   = vartable sstate
	newvartable  = M.insert key value table

-- converts a filepath in the form of expr into an absolute FilePath, using the working directory file path
evalFp :: Expr -> ScriptState -> FilePath
evalFp expr sstate = if head path == '/' then path else (wd sstate) ++ path
  where path = evalExpr expr (vartable sstate)
	
	 
evalCmdCmd :: CommandTable -> ScriptState -> Cmd -> IO ScriptState
evalCmdCmd ctable sstate cmd = do
    let vtable = vartable sstate
    let maybeCommand = M.lookup (evalExpr (name cmd)  vtable) ctable
    let ourCommand = case maybeCommand of
			  Nothing -> error "Unrecognized command"
			  Just a  -> a
    fargs <- if (isJust (inDir cmd)) then do
					let fp = evalFp (fromJust $ inDir cmd) sstate
					tempArgs <- parseFromFile (sepBy readExpr (many $ char ' ' <|> char '\t')) fp
					return $ case tempArgs of
					    Left err -> []
					    Right xs -> xs

				     else return []
    let retOut = case outDir cmd of
		      Nothing -> []
		      Just  a -> [evalFp a sstate, show (append cmd)]
    let finalArgs = (map evalExpr $ args cmd) ++ (map (`evalExpr` vtable ) fargs) ++ retOut
    maybeCommand finalArgs sstate
    