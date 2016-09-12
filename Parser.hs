module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

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

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

escapedChars :: Parser Char
escapedChars = do char '\\'
                  c <- oneOf "\\\"nrt"
                  return $ case c of 
                    '\\' -> c
                    '"'  -> c
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDecimal
          <|> parseDecimal2
          <|> parseHex
          <|> parseOct
          <|> parseBin

-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do dig <- many1 digit
                 -- return $ Number (read dig :: Integer)
-- parseNumber = many1 digit >>= return . Number . read

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= return . Number . read

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              (return . Number . fst . (!! 0) . readHex) x

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              (return . Number . fst . (!! 0) . readOct) x

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              (return . Number . bin2dec) x

bin2dec = bin2digH 0
        where bin2digH res "" = res
              bin2digH res (x : xs) = 
                let new = 2 * res + (if x == '0' then 0 else 1) in
                new `seq` bin2digH new xs

parseBool :: Parser LispVal
parseBool = do char '#'
               (char 't' >> return (Bool True)) <|>
                    (char 'f' >> return (Bool False))

parseChar :: Parser LispVal
parseChar = do try $ string "#\\"
               res <- try (string "newline" <|> string "space")
                  <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
               return $ Character $ case res of
                 "space"   -> ' '
                 "newline" -> '\n'
                 otherwise -> (res !! 0)

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float ((fst . (!! 0) . readFloat) (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal)
                  char '+'
                  y <- (try parseFloat <|> parseDecimal)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool
        <|> try parseChar

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"






