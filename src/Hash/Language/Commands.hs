-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy

module Hash.Language.Commands where

import System.IO
import System.Directory
import Hash.Language.Expressions
import Hash.Language.Exec 
import qualified Data.Map as M
import Data.List

commands :: M.Map String Command
commands =  M.fromList [("mv",mv), ("create", create) ,("rm",rm)]

takeName :: String -> String
takeName = reverse . takeWhile (/='/') . reverse

takeFolderName :: String -> String -> String
takeFolderName src target = target' ++ takeName src
  where target' = if last target /= '/' then target ++ "/" else target

foo :: IO()
foo =  putStrLn $ fst $ head $ M.toList commands


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
    mapM (flip f sstate) indivList 
    return sstate
    
mvFileToFile :: Command
mvFileToFile [src, target] sstate = do
    copyFile src target
    removeFile src
    return sstate
    
mvFileToDirectory :: Command
mvFileToDirectory [src, target] sstate = do
    let targetF = takeFolderName src target
    copyFile src targetF
    removeFile src
    return sstate
    
mvDirectoryToDirectory :: Command
mvDirectoryToDirectory [src, targetDir] sstate = do
    contents <- getDirectoryContents src
    createDirectory $ targetDir ++ "/" ++ src
    let contentsReal = map ((src++"/")++) $ delete ".." $ delete "." contents
    putStrLn $ show contentsReal;
    let indivList = zipWith (\a b -> [a,b]) contentsReal (repeat $ targetDir ++ "/"++src)
    mapM (flip mv sstate) indivList
    removeDirectoryRecursive src
    return sstate

cp :: Command
cp [] _  = error "cp: No arguments given to cp"
cp [_] _ = error "cp: Too few arguments"
cp [src,target] sstate = do
    copyFile src target
    return sstate
    
cp list sstate = do
    let target = last list
    let indivList = zipWith (\a b -> [a,takeFolderName a b]) (init list) (repeat $ target)
    mapM (flip cp sstate) indivList
    return sstate
    
    
rm :: Command
rm [] _ = error "rm: No arguments given to rm"
rm list sstate = do
    mapM removeFile list
    return sstate
    
create :: Command
create [] _ = error "create: No arguments given to create"
create [target] sstate = do
    existsF <- doesFileExist target 
    existsD <- doesDirectoryExist target
    let exists = existsF || existsD
    if exists then return sstate 
	      else do
		 openFile target WriteMode >>= hClose
		 return sstate
			  