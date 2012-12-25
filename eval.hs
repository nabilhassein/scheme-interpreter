module Eval where

import Parser
import Text.ParserCombinators.Parsec


readExpr :: String -> LispVal
readExpr input = case parse (skipMany space >> parseExpr) "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

eval :: LispVal -> LispVal
eval val@(Number _)             = val
eval val@(Complex _)            = val
eval val@(Real _)               = val
eval val@(Ratio _)              = val
eval val@(String _)             = val
eval val@(Character _)          = val
eval val@(Boolean _)            = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f : args))     = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Boolean False) ($ args) (lookup f primitives)

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+"         , numericBinOp (+))
             , ("-"        , numericBinOp (-))
             , ("*"        , numericBinOp (*))
             , ("/"        , numericBinOp div)
             , ("mod"      , numericBinOp mod)
             , ("quotient" , numericBinOp quot)
             , ("remainder", numericBinOp rem)
             , ("boolean?" , unaryOp booleanp)
             , ("string?"  , unaryOp stringp)
             , ("number?"  , unaryOp numberp)
             , ("symbol?"  , unaryOp symbolp)
             , ("list?"    , unaryOp listp)
             , ("symbol->string", unaryOp symbolToString)
             , ("string->symbol", unaryOp stringToSymbol)
             ]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String s

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s

booleanp, stringp, numberp, symbolp, listp :: LispVal -> LispVal
booleanp (Boolean _) = Boolean True
booleanp  _          = Boolean False

stringp (String _)   = Boolean True
stringp _            = Boolean False

numberp (Number _)   = Boolean True
numberp (Complex _)  = Boolean True
numberp (Real _)     = Boolean True
numberp (Ratio _)    = Boolean True
numberp  _           = Boolean False

symbolp (Atom _)     = Boolean True
symbolp  _           = Boolean False

listp (List _)       = Boolean True
listp (DottedList _ _) = Boolean True
listp  _             = Boolean False

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

--TODO: complex, real, ratio
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0