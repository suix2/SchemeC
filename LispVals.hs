module LispVals where

import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import Data.Ratio
import Data.Complex
import Data.Array

-- LispVal
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
             | PrFnc ([LispVal] -> ThrowsError LispVal)
             | Fnc {pms :: [String], argli :: (Maybe String),
                    body :: [LispVal], clsr :: Env}

instance Eq LispVal where (==) = lvTxtCmp
lvTxtCmp :: LispVal -> LispVal -> Bool
lvTxtCmp (Bool a) (Bool b) = a == b
lvTxtCmp _ _               = False

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

-- Env
type Env = IORef [(String, IORef LispVal)]

-- Error
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError
showError :: LispError -> String
showError (NumArgs expect found) = "Expected " ++ show expect
                                ++ " args: found values " ++ unList found
showError (TypeMismatch expect found) = "Invalid type: expected " ++ expect
                                     ++ ", found" ++ show found
showError (Parser pe) = "Parse error at " ++ show pe
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (UnboundVar msg var) = msg ++ ": " ++ var
showError (Default msg) = msg

-- instance Error LispError where
-- noMsg = Default "An error has occurred"
-- strMsg = Default

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO
