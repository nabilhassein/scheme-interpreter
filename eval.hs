module Main where

import LispVal
import LispError
import Primitives
import Vars
import Parser
import Text.ParserCombinators.Parsec
import Control.Monad (forM)
import Control.Monad.Error (throwError)
import System.Environment (getArgs, getProgName)
import System.IO

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Number _)             = return val
eval env val@(Complex _)            = return val
eval env val@(Real _)               = return val
eval env val@(Ratio _)              = return val
eval env val@(String _)             = return val
eval env val@(Character _)          = return val
eval env val@(Boolean _)            = return val
eval env (Atom id)                  = getEnv env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, consequent, alternative]) = do
  result <- eval env predicate
  eval env $ case result of
    Boolean False -> alternative
    _             -> consequent
-- TODO: how to retain original form in error message after recursing in cond and case?
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clause in cond expression: " form
  else case head clauses of
    List [Atom "else", expr] -> eval env expr
    List [test, expr]        -> eval env $ List [Atom "if",
                                             test,
                                             expr,
                                             List (Atom "cond" : tail clauses)]
    _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clause in case expression: " form
  else case head clauses of
    List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
    List (List datums : exprs) -> do
      result <- eval env key
      equality <- mapM (\x -> liftThrows $ eqv [result, x]) datums
      if Boolean True `elem` equality
        then mapM (eval env) exprs >>= return . last
        else eval env $ List (Atom "case" : key : tail clauses)
    _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom f : args))     = mapM (eval env) args >>= liftThrows . apply f
eval env badForm                    = throwError $ BadSpecialForm
                                      "unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "unrecognized primitive function args" f)
               ($ args)
               (lookup f primitives)


readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows . fmap show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

untilM_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untilM_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> untilM_ pred prompt action

runExpr :: String -> IO ()
runExpr expr = nullEnv >>= flip evalAndPrint expr

repl :: IO ()
repl = nullEnv >>= untilM_ (`elem` ["quit", "exit"]) (readPrompt "scheme> ") . evalAndPrint


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (skipMany space >> parseExpr) "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> repl
    1 -> runExpr $ head args
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " [expr]"

