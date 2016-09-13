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
eval (List [Atom "if", pred, conseq, alt]) = do res <- eval pred
                                                case res of
                                                    Bool True -> eval conseq
                                                    otherwise -> eval alt
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
              ("bool?", uop boolp),
              ("=", nbbop (==)),
              ("<", nbbop (<)),
              (">", nbbop (>)),
              ("/=", nbbop (/=)),
              (">=", nbbop (>=)),
              ("<=", nbbop (<=)),
              ("&&", bbbop (&&)),
              ("||", bbbop (||)),
              ("string=?", sbbop (==)),
              ("string>?", sbbop (>)),
              ("string<?", sbbop (<)),
              ("string<=?", sbbop (<=)),
              ("string>=?", sbbop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv)]

nbop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
nbop op sval@[_] = throwError $ NumArgs 2 sval
nbop op args = mapM unpackNum args >>= return . Number . foldl1 op
-- nbop op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum other      = throwError $ TypeMismatch "number" other

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

bbop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) ->
            [LispVal] -> ThrowsError LispVal
bbop unpacker op args = if length args /= 2
                         then throwError $ NumArgs 2 args
                         else do left <- unpacker $ args !! 0
                                 right <- unpacker $ args !! 1
                                 return $ Bool $ left `op` right

nbbop = bbop unpackNum
sbbop = bbop unpackStr
bbbop = bbop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr other      = throwError $ TypeMismatch "string" other

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool other    = throwError $ TypeMismatch "boolean" other

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]          = return $ List [x]
cons [x, List xs]          = return $ List $ [x] ++ xs
cons [x, DottedList xf xs] = return $ DottedList ([x] ++ xf) xs
cons [x1, x2]              = return $ DottedList [x1] x2
cons badArgList            = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]     = return $ Bool $ arg1 == arg2
eqv [(DottedList xf xs),
     (DottedList yf ys)]           = eqv [List $ xf ++ [xs], List $ yf ++ [ys]]
eqv [(List arg1), (List arg2)]     = return $
                                       Bool $
                                         (length arg1 == length arg2) &&
                                         (and $ map eqvPair $ zip arg1 arg2)
                        where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                    Left erro -> False
                                                    Right (Bool val) -> val
eqv [_, _]                         = return $ Bool False
eqv badArgList                     = throwError $ NumArgs 2 badArgList









