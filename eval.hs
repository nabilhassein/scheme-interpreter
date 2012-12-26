module Main where

import Parser
import Error
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
eval (List (Atom f : args))     = mapM eval args >>= apply f
eval badForm                    = throwError $ BadSpecialForm
                                  "unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "unrecognized primitive function args" f)
               ($ args)
               (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+"              , numericBinOp (+))
             , ("-"             , numericBinOp (-))
             , ("*"             , numericBinOp (*))
             , ("/"             , numericBinOp div)
             , ("mod"           , numericBinOp mod)
             , ("quotient"      , numericBinOp quot)
             , ("remainder"     , numericBinOp rem)
             , ("boolean?"      , unaryOp booleanp)
             , ("string?"       , unaryOp stringp)
             , ("number?"       , unaryOp numberp)
             , ("symbol?"       , unaryOp symbolp)
             , ("list?"         , unaryOp listp)
             , ("symbol->string", unaryOp symbolToString)
             , ("string->symbol", unaryOp stringToSymbol)
             ]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f []  = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp f xs  = throwError $ NumArgs 1 xs

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String s

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s

booleanp, stringp, numberp, symbolp, listp :: LispVal -> LispVal
booleanp (Boolean _)   = Boolean True
booleanp  _            = Boolean False

stringp (String _)     = Boolean True
stringp _              = Boolean False

numberp (Number _)     = Boolean True
numberp (Complex _)    = Boolean True
numberp (Real _)       = Boolean True
numberp (Ratio _)      = Boolean True
numberp  _             = Boolean False

symbolp (Atom _)       = Boolean True
symbolp  _             = Boolean False

listp (List _)         = Boolean True
listp (DottedList _ _) = Boolean True
listp  _               = Boolean False


--TODO: complex, real, ratio
numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _ []      = throwError $ NumArgs 2 []
numericBinOp _ x@[_]   = throwError $ NumArgs 2 x
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x          = throwError $ TypeMismatch "number" x



readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (skipMany space >> parseExpr) "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return . fmap show $ readExpr (head args) >>= eval
  putStrLn . extractValue $ trapError evaled