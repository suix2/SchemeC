module Main where

import System.Environment
import Control.Monad

import LispVals
import Parser
import Evaluator
import Errormsg

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractVal $ trapError evaled
