module Main where

import Hash.Hash
import Text.Parsec
import System.Environment
main :: IO()
main = do
  args <- getArgs
  case args of
       [] -> runInteractive
       [h] -> runScript h
       _  -> putStrLn "Either specify no scripts for interactive mode, or just one"
       

