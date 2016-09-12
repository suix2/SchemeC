module Evaluator where

import Parser
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array

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

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
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

nbop :: (Integer -> Integer -> Integer) -> [LispVal] ->LispVal
nbop op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0

uop :: (LispVal -> LispVal) -> [LispVal] -> LispVal
uop f [v] = f v

listp, symbolp, stringp, numberp, boolp :: LispVal -> LispVal
listp (List _)       = Bool True
listp (DottedList _) = Bool True 
listp -              = Bool False
symbolp (Atom _)     = Bool True
symbolp _            = Bool False
stringp (String _)   = Bool True
stringp _            = Bool False
numberp (Number _)   = Bool True
numberp _            = Bool False
boolp (Bool _)       = Bool True
boolp _              = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""








