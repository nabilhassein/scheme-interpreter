import Control.Monad
import Data.Char (digitToInt)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChar :: Parser Char
escapedChar = do
  char '\\'
  x <- oneOf "\"\\nrt"
  return $ case x of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> x   -- literal quote or backslash

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChar <|> noneOf "\"\\"
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

readBin :: (Integral a) => ReadS a
readBin = readInt 2 isBinaryDigit digitToInt
  where isBinaryDigit x = x == '0' || x == '1'

parseBaseNumber :: Parser LispVal
parseBaseNumber = do
  base <- oneOf "bodh"
  char '#'
  input <- many1 digit
  let number = case base of
        'b' -> fst . head . readBin $ input
        'o' -> fst . head . readOct $ input
        'd' -> read input
        'h' -> fst . head . readHex $ input
  return $ Number number

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read
-- parseNumber = do
--   input <- many1 digit
--   let number = read input
--   return $ Number number
-- parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseBaseNumber <|> parseNumber <|> parseString <|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)