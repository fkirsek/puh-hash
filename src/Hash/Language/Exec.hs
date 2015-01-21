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

import Control.Exception

-- A model of a command which is waiting for arguments and a state to run
type Command  = [String] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = String -> Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output   :: String
                               , wd       :: FilePath
                               , vartable :: VarTable
                               } deriving Show

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> TLExpr -> ScriptState -> IO ScriptState
runTopLevel ctable tlexpr sstate  = case tlexpr of
					TLCmd cmd  -> evalCmd   ctable cmd  sstate -- don't ask
					TLCnd cond -> evalCond  ctable sstate cond
					TLWh  wh   -> evalWhile ctable sstate wh
					
evalTLList :: CommandTable -> ScriptState -> [TLExpr] -> IO ScriptState
evalTLList _	   sstate []   = return sstate
evalTLList ctable sstate (h:t) = do
    newsstate <- runTopLevel ctable h sstate
    evalTLList ctable newsstate t
	
-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command’s execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr]
                  -> IO ScriptState
runHashProgram ctable eitherSstate ltlexpr = do
  let sstate = case eitherSstate of
		Left fp -> ScriptState { output = "", wd = fp, vartable = M.empty }
		Right s -> s
  evalTLList ctable sstate ltlexpr
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
	 CGT a b -> (val a >  val b)
	 CLE a b -> (val a <= val b)
	 CLT a b -> (val a <  val b)
	 CLI a   -> (not.null $ val a)
    where val a = evalExpr a vartable
	  
evalPred :: Pred -> VarTable -> Bool
evalPred pred vartable = 
    case pred of
	 Pred a   -> evalComp a vartable
	 Not a    -> not $ val a
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
-- wd must end with a /
-- used, but not needed since we use System.IO functions that get and set current directory
evalFp :: Expr -> ScriptState -> FilePath
evalFp expr sstate = evalExpr expr (vartable sstate)
{-
evalFp expr sstate = if head path == '/' then path else (wd sstate) ++ path
  where path = evalExpr expr (vartable sstate)
  -}
-- every Command takes a [String] as it's first argument
-- this function will append arguments from the file, if in redirection was enabled, and append two arguments that notify the function it should redirect it's output

catchUnknownComm :: ScriptState ->  SomeException -> IO ScriptState
catchUnknownComm sstate e = do
    putStrLn $ show e  
    return sstate

evalCmdCmd :: CommandTable -> ScriptState -> Cmd -> IO ScriptState
evalCmdCmd ctable sstate cmd = do
    let vtable = vartable sstate
    let cname = evalExpr (name cmd) vtable
    if (cname == "") 
    then return sstate 		
    else do
		let ourCommand = ctable cname
		fargs <- if (isJust (inDir cmd)) then do
						    let fp = evalFp (fromJust $ inDir cmd) sstate
						    tempArgs <- parseExprFromFile fp
						    return $ case tempArgs of
							Left err -> []
							Right xs -> xs

						else return []
		let fargsEvaluated = map (`evalExpr` vtable ) fargs
		let finalArgs = (map (`evalExpr` vtable ) $ args cmd) ++ fargsEvaluated
		newsstate <- catch (ourCommand finalArgs sstate) $ catchUnknownComm (sstate{output =""}) 
		let retOut = outDir cmd
		case retOut of
		    Nothing -> putStr (output newsstate)
		    Just fp -> case append cmd of
			      True -> appendFile (evalFp fp sstate) (output newsstate)
			      _    -> writeFile  (evalFp fp sstate) (output newsstate)
		return newsstate

-- here, ScriptState is the last argument to allow easier chaining
evalCmd :: CommandTable -> Cmd -> ScriptState -> IO ScriptState
evalCmd ctable cmd sstate  =
  case cmd of
       Assign _ _ -> return $ evalCmdAssign cmd sstate
       Cmd    _ _ _ _ _-> evalCmdCmd ctable sstate cmd
       
evalCmdList :: CommandTable -> ScriptState -> [Cmd] -> IO ScriptState
evalCmdList _	   sstate []   = return sstate
evalCmdList ctable sstate (h:t) = do
    newsstate <- evalCmd ctable h sstate
    evalCmdList ctable newsstate t
    
evalCondIfThenElse :: CommandTable -> ScriptState -> Conditional -> IO ScriptState
evalCondIfThenElse ctable sstate (IfElse cond1 cthen1 celse1) = if evalPred cond1 (vartable sstate) 
								   then evalCmdList ctable sstate cthen1
								else evalCmdList ctable sstate celse1
				
evalCond :: CommandTable -> ScriptState -> Conditional -> IO ScriptState
evalCond ctable sstate conditional = case conditional of
					  If cond1 cthen1 -> evalCondIfThenElse ctable sstate (IfElse {cond = cond1, cthen=cthen1, celse = []} )
					  IfElse cond1 cthen1 celse1 -> evalCondIfThenElse ctable sstate conditional
					  
evalWhile :: CommandTable -> ScriptState -> While -> IO ScriptState
evalWhile ctable sstate while = 
      case evalPred (cnd while) (vartable sstate) of
	   True ->  do
		    newsstate <- evalCmdList ctable sstate (comm while)
		    evalWhile ctable newsstate while
	   False -> return sstate