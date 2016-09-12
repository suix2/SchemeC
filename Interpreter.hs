module Main where

import System.Environment
import Parser
import Evaluator

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
