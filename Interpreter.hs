module Main where

import System.Environment
import Control.Monad
import System.IO

import LispVals
import Parser
import Evaluator
import Errormsg

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> ept $ args !! 0
            _ -> putStrLn "Program takes only 0 or 1 args."

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractVal $ trapError $ liftM show $ readExpr expr
                    >>= eval

ept :: String -> IO ()
ept expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt act = do res <- prompt
                            if pred res
                              then return ()
                              else act res >> until_ pred prompt act

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") ept









