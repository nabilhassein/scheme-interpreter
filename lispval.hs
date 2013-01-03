module LispVal where

import Data.Ratio
import Data.Complex
import Numeric

data LispVal = List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Int, [LispVal])
             | Number Integer
             | Complex (Complex Double)
             | Real Double
             | Ratio Rational
             | String String
             | Character Char
             | Boolean Bool
             | Atom String
             deriving (Eq)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (List xs)         = "(" ++ unwordsList xs ++ ")"
showVal (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ showVal x ++ ")"
showVal (Vector (n, xs))  = "#(" ++ unwordsList xs ++ ")"
showVal (Number n)        = show n
showVal (Complex c)       = show (realPart c) ++ show (imagPart c)
showVal (Real r)          = show r
showVal (Ratio n)         = show (numerator n) ++ "/" ++ show (denominator n)
showVal (String s)        = "\"" ++ s ++ "\""
showVal (Character c)     = "'" ++ [c] ++ "'"
showVal (Boolean True)    = "#t"
showVal (Boolean False)   = "#f"
showVal (Atom a)          = a

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


