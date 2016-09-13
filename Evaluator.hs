module Evaluator where

import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad.Except

import LispVals
import Errormsg

-- data LispVal = Atom String
             -- | List [LispVal]
             -- | DottedList [LispVal] LispVal
             -- | Number Integer
             -- | String String
             -- | Bool Bool
             -- | Character Char
             -- | Float Double
             -- | Ratio Rational
             -- | Complex (Complex Double)
             -- | Vector (Array Int LispVal)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
-- eval (List (Atom func : args)) = apply func $ map eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction
                                      "Unrecognized Function"
                                      func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", nbop (+)),
              ("-", nbop (-)),
              ("*", nbop (*)),
              ("/", nbop div),
              ("mod", nbop mod),
              ("quotient", nbop quot),
              ("remainder", nbop rem),
              ("list?", uop listp),
              ("symbol?", uop symbolp),
              ("string?", uop stringp),
              ("number?", uop numberp),
              ("bool?", uop boolp)]

nbop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
nbop op sval@[_] = throwError $ NumArgs 2 sval
nbop op args = mapM unpackNum args >>= return . Number . foldl1 op
-- nbop op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum s          = throwError $ TypeMismatch "number" s

uop :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
uop f [v] = return $ f v
uop _ a   = throwError $ NumArgs 1 a

listp, symbolp, stringp, numberp, boolp :: LispVal -> LispVal
listp (List _)         = Bool True
listp (DottedList _ _) = Bool True 
listp _                = Bool False
symbolp (Atom _)       = Bool True
symbolp _              = Bool False
stringp (String _)     = Bool True
stringp _              = Bool False
numberp (Number _)     = Bool True
numberp _              = Bool False
boolp (Bool _)         = Bool True
boolp _                = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""








