module LispVal where

import Text.ParserCombinators.Parsec
import Control.Monad.Error
import Data.IORef
import Data.Ratio
import Data.Complex
import Numeric
import System.IO

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
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: EnvRef }

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
showVal (Port _)          = "<IO Port>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _)        = "<IO primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
            "(lambda (" ++ unwords (map show args) ++
            case varargs of
              Nothing -> ""
              Just arg -> " . " ++ arg
            ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal



data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

instance Error LispError where
  noMsg  = Default "An error has occurred"
  strMsg = Default

instance Show LispError where
  show = showError

showError :: LispError -> String
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                          ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (Default string)              = string


extractValue :: ThrowsError a -> a
extractValue (Right val) = val



type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = action `catchError` (return . show)

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue


type EnvRef = IORef [(String, IORef LispVal)]

getVar :: EnvRef -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: EnvRef -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value

isBound :: EnvRef -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

defineVar :: EnvRef -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value
    else liftIO $ do 
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: EnvRef -> [(String, LispVal)] -> IO EnvRef
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef

extendEnv :: [(String, LispVal)] -> [(String, IORef LispVal)] -> IO [(String, IORef LispVal)]
extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)

addBinding :: (String, LispVal) -> IO (String, IORef LispVal)
addBinding (var, value) = do ref <- newIORef value
                             return (var, ref)

