-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy

module Hash.Language.Commands where

import System.IO
import System.Directory
import Hash.Language.Expressions
import Hash.Language.Exec 
import qualified Data.Map as M
import Data.List
import Hexdump
import qualified Data.ByteString.Char8 as BS

-- for echo
import Text.Parsec.String
import Text.ParserCombinators.Parsec
import Hash.Parsing.HashParser

scriptDirectory :: FilePath
scriptDirectory = "/hash/bin"

commandsMap :: M.Map String Command
commandsMap =  M.fromList [ ("mv",mv), ("cp",cp), ("create", create) ,("rm",rm), ("cpdir", cpdir)
			, ("mkdir",mkdir), ("rmdir", rmdir), ("ls", ls)
			, ("pwd", pwd), ("cd",cd), ("echo", echo), ("cat", cat)
			, ("quit", quit)
			, ("hexdump", hexd)]

commands :: String -> Command
commands cname = case M.lookup cname commandsMap of
		      Just a -> a
		      Nothing -> error "Unrecognized command"

commandFromFile :: String -> Command
commandFromFile fp args sstate = do
  gh <- getHomeDirectory
  let script = gh ++ scriptDirectory ++ "/" ++ fp ++ ".hash"
  ltlexpr <- parseTLExprsFromFile script args
  rez <- runHashProgram commands (Right sstate) ltlexpr
  return rez
  
quit :: Command
quit _ sstate =
  return $ sstate{ output = "", wd = ";quit"}

takeName :: String -> String
takeName = reverse . takeWhile (/='/') . reverse

takeFolderName :: String -> String -> String
takeFolderName src target = target' ++ takeName src
  where target' = if last target /= '/' then target ++ "/" else target
{-
foo :: IO()
foo =  putStrLn $ fst $ head $ M.toList commands
-}

mv  :: Command
mv []  _ = error "mv: No arguments given"
mv [_] _ = error "mv: Too few arguments"

mv [src,target] sstate = do
  srcF <- doesFileExist src
  srcD <- doesDirectoryExist src
  targetF <- doesFileExist target
  targetD <- doesDirectoryExist target
  case srcF of
       True -> case targetD of
		    False -> mvFileToFile [src,target] sstate
		    True -> mvFileToDirectory [src, target] sstate
       False-> case srcD of
		    True ->  renameDirectory src target >> return sstate
		    False -> error "mv: File doesn't exist!"
mv list sstate = do
    targetD <- doesDirectoryExist (last list)
    let f = case targetD of
	 True  -> mvDirectoryToDirectory
	 False -> mv
    let indivList = zipWith (\a b -> [a,b]) (init list) (repeat $ last list)
    mapM_ (flip f sstate) indivList 
    return $ sstate { output=""}
    
mvFileToFile :: Command
mvFileToFile [src, target] sstate = do
    copyFile src target
    removeFile src
    return $ sstate { output=""}
    
mvFileToDirectory :: Command
mvFileToDirectory [src, target] sstate = do
    let targetF = takeFolderName src target
    copyFile src targetF
    removeFile src
    return $ sstate { output=""}
    
mvDirectoryToDirectory :: Command
mvDirectoryToDirectory [src, targetDir] sstate = do
    contents <- getDirectoryContents src
    createDirectory $ targetDir ++ "/" ++ src
    let contentsReal = map ((src++"/")++) $ delete ".." $ delete "." contents
    putStrLn $ show contentsReal;
    let indivList = zipWith (\a b -> [a,b]) contentsReal (repeat $ targetDir ++ "/"++src)
    mapM_ (flip mv sstate) indivList
    removeDirectoryRecursive src
    return $ sstate { output=""}

cp :: Command
cp [] _  = error "cp: No arguments given to cp"
cp [_] _ = error "cp: Too few arguments"
cp [src,target] sstate = do
    copyFile src target
    return $ sstate { output=""}
    
cp list sstate = do
    let target = last list
    let indivList = zipWith (\a b -> [a,takeFolderName a b]) (init list) (repeat $ target)
    mapM_ (flip cp sstate) indivList
    return $ sstate { output=""}
    
    
rm :: Command
rm [] _ = error "rm: No arguments given to rm"
rm list sstate = do
    mapM_ removeFile list
    return $ sstate { output=""}
    
create :: Command
create [] _ = error "create: No arguments given to create"
create [target] sstate = do
    existsF <- doesFileExist target 
    existsD <- doesDirectoryExist target
    let exists = existsF || existsD
    if exists then return $ sstate { output=""}
	      else do
		 openFile target WriteMode >>= hClose
		 return $ sstate { output=""}

cpdir :: Command
cpdir [] _ = error "cpdir: No arguments given"
cpdir [_] _= error "cpdir: No arguments given"
cpdir list sstate = do
    let indivList = zipWith (\a b -> [a,b] ) (init list) (repeat $ last list)
    mapM_ (flip cpDirectoryIntoDirectory sstate) indivList
    return $ sstate { output=""}
    
cpDirectoryIntoDirectory :: Command
cpDirectoryIntoDirectory [src, targetDir] sstate = do
    contents <- getDirectoryContents src
    createDirectory $ targetDir ++ "/" ++ src
    let contentsReal = map ((src++"/")++) $ delete ".." $ delete "." contents
    putStrLn $ show contentsReal;
    let indivList = zipWith (\a b -> [a,b]) contentsReal (repeat $ targetDir ++ "/"++src)
    mapM_ (flip mv sstate) indivList
    return $ sstate{ output=""}

mkdir :: Command
mkdir [] _ = error "mkdir: No arguments given"
mkdir [target] sstate = createDirectory target >> (return $ sstate { output=""} )
mkdir list sstate = mapM_(\x -> mkdir [x] sstate) list >> (return $ sstate { output=""} )

rmdir :: Command
rmdir [] _ = error "rmdir: No arguments given"
rmdir [target] sstate = do
    con <- getDirectoryContents target
    let content =  delete ".." $ delete "." con 
    case content of
	 [] -> removeDirectory target >> (return $ sstate { output=""})
	 _  -> error "Directory isn't empty"
    
ls :: Command
ls [] sstate = ls ["."] sstate
ls list sstate = do
    con <- getDirectoryContents (head list)
    return $ sstate { output = unlines$con }
    
pwd :: Command
pwd [] sstate = do
    con <- getCurrentDirectory
    return $ sstate { output = con ++ "\n"}
    
pwd _ _ = error "pwd: Too many arguments"
    
cd :: Command
cd [] _ = error "cd: No arguments given"
cd [target] sset = do
    cur <- getCurrentDirectory
    setCurrentDirectory target
    return $ sset { wd = cur, output = "" }
  
cd _ _ = error "cd: Too many arguments given"

readStrNotVar :: Parser Expr
readStrNotVar = do
    str <- many1 $ noneOf "$"
    return $ Str str
    
readStrAnything :: Parser Expr
readStrAnything =  do
    ch <- noneOf "\n"
    return $  Str [ch]


echo :: Command
echo [] sstate = return $ sstate{output = "\n"}
echo targets  sstate = do
    let target = concat targets
    let vals  = parse (many $ try readExprVar <|> readStrAnything ) "echo" target
    let vals2 = case vals of
		Left err -> error "echo: somehow the parse failed"
		Right a  -> a
    let out = concat $ map (flip evalExpr (vartable sstate) ) vals2
    return $ sstate { output = out ++ "\n" }
    
cat :: Command
cat [] sstate = error "cat: No arguments given"
cat list sstate = do
    ress <- mapM cat' list
    return $ sstate { output = unlines $ ress }
    
cat' :: FilePath -> IO String
cat' src = readFile src
    
hexd :: Command
hexd list sstate = do
    con <- BS.readFile (head list)
    let pretty = simpleHex con 
    putStrLn pretty
    return sstate{output = pretty}

  