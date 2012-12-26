module Primitives where

import Parser
import Error
import Text.ParserCombinators.Parsec
import Control.Monad.Error (throwError)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+"             , numericBinOp (+))
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
             , ("="             , numBoolBinOp (==))
             , ("<"             , numBoolBinOp (<))
             , (">"             , numBoolBinOp (>))
             , ("/="            , numBoolBinOp (/=))
             , (">="            , numBoolBinOp (>=))
             , ("<="            , numBoolBinOp (<=))
             , ("&&"            , boolBoolBinOp (&&))
             , ("||"            , boolBoolBinOp (||))
             , ("string=?"      , strBoolBinOp (==))
             , ("string<?"      , strBoolBinOp (<))
             , ("string>?"      , strBoolBinOp (>))
             , ("string<=?"     , strBoolBinOp (<=))
             , ("string>=?"     , strBoolBinOp (>=))
             ]

--TODO: complex, real, ratio
numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params = case length params of
  2 -> mapM unpackNum params >>= return . Number . foldl1 op
  _ -> throwError $ NumArgs 2 params


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x          = throwError $ TypeMismatch "number" x

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr x          = throwError $ TypeMismatch "string" x

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Boolean b) = return b
unpackBool x           = throwError $ TypeMismatch "boolean" x


unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f []  = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp f xs  = throwError $ NumArgs 1 xs

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

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String s

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s


boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = case length args of
  2 -> do
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return . Boolean $ left `op` right
  _ -> throwError $ NumArgs 2 args

numBoolBinOp  = boolBinOp unpackNum
strBoolBinOp  = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool


car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [x]                   = throwError $ TypeMismatch "pair" x
car xs                    = throwError $ NumArgs 1 xs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [x]                   = throwError $ TypeMismatch "pair" x
cdr xs                    = throwError $ NumArgs 1 xs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]         = return $ List [x]
cons [x, List xs]         = return $ List (x:xs)
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y]               = return $ DottedList [x] y
cons x                    = throwError $ NumArgs 2 x

eqv :: [LispVal] -> ThrowsError LispVal
eqv = undefined