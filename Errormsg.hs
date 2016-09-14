module Errormsg where

import Control.Monad.Except

import LispVals

trapError action = catchError action (return . show)

extractVal :: ThrowsError a -> a
extractVal (Right val) = val
