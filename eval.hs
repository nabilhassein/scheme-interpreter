module Main where

import Parser
import Error
import Primitives
import Text.ParserCombinators.Parsec
import Control.Monad.Error (throwError)
import System.Environment (getArgs, getProgName)
import System.IO

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


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return . extractValue . trapError $ fmap show $ readExpr expr >>= eval

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

untilM_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untilM_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> untilM_ pred prompt action

repl :: IO ()
repl = untilM_ (`elem` ["quit", "exit"]) (readPrompt "scheme> ") evalAndPrint


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (skipMany space >> parseExpr) "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> repl
    1 -> evalAndPrint $ head args
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " [expr]"
--  evaled <- return . fmap show $ readExpr (head args) >>= eval
--  putStrLn . extractValue $ trapError evaled

