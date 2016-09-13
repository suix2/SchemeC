module NameTable where

import Data.IORef

import LispVals

type Env = IORef [(String, IORef LispVal)]








