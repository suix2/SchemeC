module NameTable where

import Data.IORef
import Control.Monad.Except

import LispVals
import Errormsg

type Env = IORef [(String, IORef LispVal)]

newEnv :: IO Env
newEnv = newIORef []

liftTE :: ThrowsError a -> IOThrowsError a
liftTE (Left err)  = throwError err
liftTE (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows act = runExceptT (trapError act) >>= return . extractVal

isBound :: Env -> String -> IO Bool
isBound env var = readIORef env >>=
                    return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar env var = do res <- liftIO $ readIORef env
                    maybe (throwError $ UnboundVar "Var unbound" var)
                          (liftIO . readIORef)
                          (lookup var res)

serVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar env var val = do res <- liftIO $ readIORef env
                        maybe (throwError $ UnboundVar "Var unbound" var)
                              (liftIO . (flip writeIORef val))
                              (lookup var res)
                        return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar env var val = do dfnd <- liftIO $ isBound env var
                           if dfnd
                              then setVar env var val >> return val
                              else liftIO $ do valref <- newIORef val
                                               res <- readIORef env
                                               writeIORef env
                                                          ((var, valref) : res)
                                               return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars env bounds = readIORef env >>= extendEnv bounds >>= newIORef
        where extendEnv bounds env  = liftM (++ env) (mapM addBinding bounds)
              addBinding (var, val) = do ref <- newIORef val
                                         return (val, ref)










