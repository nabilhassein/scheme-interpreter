import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.Char (digitToInt)
import Data.Ratio
import Data.Complex
import Numeric


data LispVal = List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Complex (Complex Double)
             | Real Double
             | Rational Rational
             | String String
             | Character Char
             | Boolean Bool
             | Atom String
             deriving Show

-- parseRational :: Parser LispVal
-- parseRational = do
  
  


parseBoolean :: Parser LispVal
parseBoolean = do
  char '#'
  x <- oneOf "tf"
  return $ case x of
    't' -> Boolean True
    'f' -> Boolean False


parseReal :: Parser LispVal
parseReal = do
  i <- many1 digit
  char '.'
  f <- many1 digit
  return . Real . fst . head . readFloat $ i ++ "." ++ f


parseNumber :: Parser LispVal
parseNumber = parseDig1 <|> parseDig2 <|> parseBin <|> parseOct <|> parseHex

parseDig1 :: Parser LispVal
parseDig1 = many1 digit >>= return . Number . read

parseDig2 :: Parser LispVal
parseDig2 = do
  string "d#" <|> string "D#"
  x <- many1 digit
  return . Number . read $ x

parseBin :: Parser LispVal
parseBin = do
  string "b#" <|> string "B#"
  x <- many1 $ oneOf "01"
  return . Number . fst . head . readBin $ x
  where readBin = readInt 2 isBinaryDigit digitToInt
        isBinaryDigit x = x `elem` "01"

parseOct :: Parser LispVal
parseOct = do
  string "o#" <|> string "O#"
  x <- many1 octDigit
  return . Number . fst . head . readOct $ x

parseHex :: Parser LispVal
parseHex = do
  string "h#" <|> string "H#"
  x <- many1 hexDigit
  return . Number . fst . head . readHex $ x


characterNames :: [(String, Char)]
characterNames = [ ("nul", '\NUL')
                 , ("alarm", '\BEL')
                 , ("backspace", '\BS')
                 , ("tab", '\t')
                 , ("linefeed", '\n')
                 , ("newline", '\n')
                 , ("vtab", '\v')
                 , ("page", '\f')
                 , ("return", '\CR')
                 , ("esc", '\ESC')
                 , ("space", ' ')
                 , ("delete", '\DEL')
                 ]

tryAll :: [(String, Char)] -> [GenParser Char st String]
tryAll xs = [try (string s) | s <- strings xs]
  where strings :: [(String, Char)] -> [String]
        strings = map fst

parseNamedChar :: Parser LispVal
parseNamedChar = do
  string "#\\"
  x <- foldr1 (<|>) (tryAll characterNames)
  return . Character . fromJust $ lookup x characterNames

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  anyChar >>= return . Character


escapedChar :: Parser Char
escapedChar = do
  char '\\'
  x <- oneOf "\"\\nrt"
  return $ case x of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> x   -- literal quote or backslash

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  return . Atom $ first:rest


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChar <|> noneOf "\"\\"
  char '"'
  return $ String x


parseExpr :: Parser LispVal
parseExpr = try parseBoolean <|>
            try parseReal <|> 
            try parseNumber <|>
            try parseNamedChar <|>
            try parseChar <|>
            try parseAtom <|> 
            parseString

readExpr :: String -> String
readExpr input = case parse (skipMany space >> parseExpr) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val


main :: IO ()
main = do
  args <- getArgs
  putStrLn . readExpr $ args !! 0