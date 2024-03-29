module Parser where

import LispVal
import Text.ParserCombinators.Parsec
import Data.Maybe (fromJust)
import Data.Char (digitToInt)
import Data.Ratio
import Data.Complex
import Numeric

-- TODO: add semicolon as comment

parseBoolean :: Parser LispVal
parseBoolean = do
  char '#'
  x <- oneOf "tf"
  return $ case x of
    't' -> Boolean True
    'f' -> Boolean False


parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseReal <|> parseNumber
  skipMany space
  char '+'
  skipMany space
  y <- try parseReal <|> parseNumber
  char 'i'
  let a = toDouble x
      b = toDouble y
  return . Complex $ a :+ b
  where toDouble :: LispVal -> Double
        toDouble (Real n) = n
        toDouble (Number n) = fromIntegral n


parseReal :: Parser LispVal
parseReal = do
  i <- many1 digit
  char '.'
  f <- many1 digit
  return . Real . fst . head . readFloat $ i ++ "." ++ f


parseRatio :: Parser LispVal
parseRatio = do
  n <- many1 digit
  char '/'
  d <- many1 digit
  return . Ratio $ (read n) % (read d)
  

--TODO: exact and inexact; precision
parseNumber :: Parser LispVal
parseNumber = parseDig1 <|>
              parseDig2 <|>
              parseBin <|>
              parseOct <|>
              parseHex

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
tryAll xs = map (try . string) (strings xs)
  where strings :: [(String, Char)] -> [String]
        strings = map fst

parseNamedChar :: Parser LispVal
parseNamedChar = do
  string "#\\"
  x <- foldr1 (<|>) (tryAll characterNames)
  return . Character . fromJust $ lookup x characterNames

parseRegularChar :: Parser LispVal
parseRegularChar = do
  string "#\\"
  anyChar>>= return . Character

parseChar :: Parser LispVal
parseChar = try parseNamedChar <|> parseRegularChar


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  return . Atom $ first:rest


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


parseQuote :: Parser LispVal
parseQuote = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


--TODO: commas, etc.
parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]


parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]


spaces1 :: Parser ()
spaces1 = skipMany1 space

parseList :: Parser LispVal
parseList = do
  char '('
  x <- fmap List $ sepBy parseExpr spaces1
  char ')'
  return x


parseDottedList :: Parser LispVal
parseDottedList = do
  char '('
  x <- endBy parseExpr spaces1
  char '.'
  spaces1
  xs <- parseExpr
  char ')'
  return $ DottedList x xs


parseVector :: Parser LispVal
parseVector = do
  string "#("
  x <- sepBy parseExpr spaces1
  char ')'
  return $ Vector (length x, x)


parseExpr :: Parser LispVal
parseExpr = try parseBoolean <|>
            try parseComplex <|>
            try parseReal <|> 
            try parseRatio <|>
            try parseNumber <|>
            try parseChar <|>
            try parseAtom <|> 
            try parseString <|>
            try parseQuote <|>
            try parseUnquote <|>
            try parseQuasiquote <|>
            try parseList <|>
            try parseDottedList <|>
            try parseVector

