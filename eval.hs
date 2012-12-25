module Evaluator where

import Parser
import Data.Complex (realPart, imagPart)
import Data.Ratio (numerator, denominator)

showVal :: LispVal -> String
showVal (List xs)         = "(" ++ concatMap showVal xs ++ ")"
showVal (DottedList xs x) = "(" ++ concatMap showVal xs ++ showVal x ++ ")"
showVal (Vector (n, xs))  = "#(" ++ concatMap showVal xs ++ ")"
showVal (Number n)        = show n
showVal (Complex c)       = show (realPart c) ++ show (imagPart c)
showVal (Real r)          = show r
showVal (Ratio n)         = show (numerator n) ++ "/" ++ show (denominator n)
showVal (String s)        = "\"" ++ s ++ "\""
showVal (Character c)     = "'" ++ [c] ++ "'"
showVal (Boolean True)    = "#t"
showVal (Boolean False)   = "#f"
showVal (Atom a)          = a
