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
commands =  M.fromList []

takeName :: String -> String
takeName = reverse . takeWhile (/='/') . reverse

mv  :: Command
mv [src,target] sstate = do
  srcF <- doesFileExist src
  srcD <- doesDirectoryExist src
  targetF <- doesFileExist target
  targetD <- doesDirectoryExist target
  case srcF of
       True -> case targetF of
		    True -> mvFileToFile [src,target] sstate
		    _ -> if targetD then mvFileToDirectory [src, target] sstate else error "target directory doesn't exist"
       False-> case targetD of
		    True ->  mvDirectoryToDirectory [src,target] sstate
		    _ -> error "Can't move a directory there"
mv list sstate = do
    let indivList = zipWith (\a b -> [a,b]) (init list) (repeat $ last list)
    mapM (flip mv sstate) indivList 
    return sstate
    
mvFileToFile :: Command
mvFileToFile [src, target] sstate = do
    copyFile src target
    removeFile src
    return sstate
    
mvFileToDirectory :: Command
mvFileToDirectory [src, target] sstate = do
    let target' = if last target /= '/' then target ++ "/" else target
    let targetF = target' ++ takeName src
    copyFile src targetF
    removeFile src
    return sstate
    
mvDirectoryToDirectory :: Command
mvDirectoryToDirectory [src, targetDir] sstate = do
    contents <- getDirectoryContents src
    let contentsReal = delete ".." $ delete "." contents
    let indivList = zipWith (\a b -> [a,b]) contentsReal (repeat targetDir)
    mapM (flip mv sstate) indivList
    return sstate
    

    
cp :: Command
cp [src,target] sstate = do
    let target' = if last target /= '/' then target++"/" else target
    copyFile src target'
    return sstate

cp list sstate = do
    let indivList = zipWith (\a b -> [a, b ++ takeName a] ) (init list) (repeat $ last list)
    mapM (flip cp sstate) indivList 
    return sstate
    
rm :: Command
rm list sstate = do
    mapM removeFile list
    return sstate
    
create :: Command
create [target] sstate = do
    existsF <- doesFileExist target 
    existsD <- doesDirectoryExist target
    let exists = existsF || existsD
    if exists then return sstate 
	      else do
		 openFile target WriteMode >>= hClose
		 return sstate
			  