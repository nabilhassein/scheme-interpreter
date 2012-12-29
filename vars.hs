module Vars where

import Parser
import Error
import Control.Monad.Error
import Data.IORef

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO


nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

