module LispVals where

import Data.Ratio
import Data.Complex
import Data.Array

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (String str)       = "\"" ++ str ++ "\""
showVal (Atom name)        = name
showVal (Number num)       = show num
showVal (Bool True)        = "#t"
showVal (Bool False)       = "#f"
showVal (List li)          = "(" ++ unList li ++ ")"
showVal (DottedList f s)   = "(" ++ unList f ++ " . " ++ showVal s ++ ")"
showVal (Character c)      = [c]
showVal (Float f)          = show f
showVal (Ratio r)          = show (numerator r) ++ "/" ++ show (denominator r)
showVal (Complex (r :+ c)) = show r ++ "+" ++ show c ++"i"
showVal (Vector arr)       = "(vector " ++ unList (elems arr) ++ ")"

unList :: [LispVal] -> String
unList = unwords . map showVal
