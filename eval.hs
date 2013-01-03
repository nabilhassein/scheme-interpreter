module Main where

import LispVal
import Primitives
import Parser
import Text.ParserCombinators.Parsec
import Control.Monad (forM)
import Control.Monad.Error (throwError, liftIO)
import System.Environment (getArgs, getProgName)
import System.IO


makeFunc :: Maybe String -> EnvRef -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: EnvRef -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> EnvRef -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . showVal


eval :: EnvRef -> LispVal -> IOThrowsError LispVal
-- TODO: quasiquote, unquote, vector
eval env val@(Number _)             = return val
eval env val@(Complex _)            = return val
eval env val@(Real _)               = return val
eval env val@(Ratio _)              = return val
eval env val@(String _)             = return val
eval env val@(Character _)          = return val
eval env val@(Boolean _)            = return val
eval env (Atom id)                  = getVar env id
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

-- TODO: rewrite case without using LispVal deriving Eq
-- eval env form@(List (Atom "case" : key : clauses)) =
--   if null clauses
--   then throwError $ BadSpecialForm "no true clause in case expression: " form
--   else case head clauses of
--     List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
--     List (List datums : exprs) -> do
--       result <- eval env key
--       equality <- mapM (\x -> liftThrows $ eqv [result, x]) datums
--       if Boolean True `elem` equality
--         then mapM (eval env) exprs >>= return . last
--         else eval env $ List (Atom "case" : key : tail clauses)
--     _ -> throwError $ BadSpecialForm "ill-formed case expression: " form

eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body

eval env (List (f : args)) = do
  func <- eval env f
  argVals <- mapM (eval env) args
  apply func argVals

eval env badForm = throwError $ BadSpecialForm "unrecognized special form" badForm


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
  then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = fmap last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Nothing       -> return env
          Just argName  -> liftIO $ bindVars env [(argName, List remainingArgs)]
apply x _ = throwError $ NotFunction "not a function: " (show x)


primitiveBindings :: IO EnvRef
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, f) = (var, PrimitiveFunc f)

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalString :: EnvRef -> String -> IO String
evalString env expr = runIOThrows . fmap show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: EnvRef -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

untilM_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untilM_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> untilM_ pred prompt action

runExpr :: String -> IO ()
runExpr expr = primitiveBindings >>= flip evalAndPrint expr

repl :: IO ()
repl = primitiveBindings >>= untilM_ (`elem` ["quit", "exit"]) (readPrompt "scheme> ") . evalAndPrint


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (skipMany space >> parseExpr) "scheme" input of
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

