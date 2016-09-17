module Main where

import System.Environment
import Control.Monad
import System.IO

import LispVals
import Parser
import Evaluator
import Errormsg
import NameTable

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runSngl args
-- main = do args <- getArgs
          -- case length args of
            -- 0 -> runRepl
            -- 1 -> runSngl $ args !! 0
            -- _ -> putStrLn "Program takes only 0 or 1 args."

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftTE $ readExpr expr)
                        >>= eval env
-- evalString expr = return $ extractVal $ trapError $ liftM show $ readExpr expr
                    -- >>= eval

ept :: Env -> String -> IO ()
ept env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt act = do res <- prompt
                            if pred res
                              then return ()
                              else act res >> until_ pred prompt act

runRepl :: IO ()
runRepl = initPrEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . ept
-- runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") ept

runSngl :: [String] -> IO ()
runSngl args = do 
        env <- initPrEnv >>=
            flip bindVars [("args", List $ map String $ drop 1 args)]
        (runIOThrows $ liftM show $ eval env (List [Atom "load",
                                                    String (args !! 0)])) >>=
            hPutStrLn stderr
-- runSngl expr = initPrEnv >>= flip ept expr







