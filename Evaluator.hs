module Evaluator where

import Parser

showVal :: LispVal -> String
showVal (String str) = "\"" ++ str ++ "\""
showVal (Atom name)  = name
showVal (Number num) = show num
showVal (Bool True)  = "#t"
showVal (Bool False) = "#f"












