module Main where

import Hash.Hash
import Text.Parsec
import System.Environment
main :: IO()
main = do
  args <- getArgs
  case args of
       [] -> runInteractive
       (h:t) -> runScript h t       

