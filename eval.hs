module Main where

import Parser
import Error
import Primitives
import Text.ParserCombinators.Parsec
import Control.Monad.Error (throwError)
import System.Environment (getArgs)

eval :: LispVal -> ThrowsError LispVal
eval val@(Number _)             = return val
eval val@(Complex _)            = return val
eval val@(Real _)               = return val
eval val@(Ratio _)              = return val
eval val@(String _)             = return val
eval val@(Character _)          = return val
eval val@(Boolean _)            = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, consequent, alternative]) = do
  result <- eval predicate
  eval $ case result of
    Boolean False -> alternative
    _             -> consequent
eval (List (Atom f : args))     = mapM eval args >>= apply f
eval badForm                    = throwError $ BadSpecialForm
                                  "unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "unrecognized primitive function args" f)
               ($ args)
               (lookup f primitives)


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (skipMany space >> parseExpr) "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return . fmap show $ readExpr (head args) >>= eval
  putStrLn . extractValue $ trapError evaled

