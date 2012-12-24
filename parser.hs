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
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

characterNames :: [String]
characterNames = [ "nul"
                 , "alarm"
                 , "backspace"
                 , "tab"
                 , "linefeed"
                 , "newline"
                 , "vtab"
                 , "page"
                 , "return"
                 , "esc"
                 , "space"
                 , "delete"
                 ]

parseBool :: Parser LispVal
parseBool = do
  char '#'
  x <- oneOf "tf"
  return $ case x of
    't' -> Bool True
    'f' -> Bool False


parseChar :: Parser LispVal
parseChar = undefined

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
  return . Atom $ first:rest


parseNumber :: Parser LispVal
parseNumber = parseDig1 <|> parseDig2 <|> parseBin <|> parseOct <|> parseHex

parseDig1 :: Parser LispVal
parseDig1 = many1 digit >>= return . Number . read

parseDig2 :: Parser LispVal
parseDig2 = do
  try $ string "d#"
  x <- many1 digit
  return . Number . read $ x

parseBin = do
  try $ string "b#"
  x <- many1 $ oneOf "01"
  return . Number . fst . head . readBin $ x
  where readBin = readInt 2 isBinaryDigit digitToInt
        isBinaryDigit x = x `elem` "01"

parseOct = do
  try $ string "o#"
  x <- many1 octDigit
  return . Number . fst . head . readOct $ x

parseHex = do
  try $ string "h#"
  x <- many1 hexDigit
  return . Number . fst . head . readHex $ x


parseExpr :: Parser LispVal
parseExpr = parseBool <|> parseAtom <|> parseNumber <|> parseString <|> parseChar

readExpr :: String -> String
readExpr input = case parse (skipMany space >> parseExpr) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val


main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)