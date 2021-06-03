{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Syntax
import Parsing
import Pretty
import Evaluator
import Examples
import TypeChecker
import Control.Monad
import Data.String (IsString(..))

main :: IO ()
main = 
  readLn  >>= \x ->
  putStrLn x >> 
  return (parseFMC x) >>= \y ->
  putStrLn (show y) >>
  return (eval y) >>= \z ->
  putStrLn (printStack z)  >>
  return ()
{-

  do putStrLn "Hello!" >> sepparate
          putStrLn peex2
  where
    sepparate = putStrLn $ take 80 $ repeat '-'
-}
