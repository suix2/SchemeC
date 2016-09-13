module Errormsg where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

import LispVals

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

trapError action = catchError action (return . show)

extractVal :: ThrowsError a -> a
extractVal (Right val) = val










