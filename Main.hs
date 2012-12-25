module Main where

import Parser
import Error
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head