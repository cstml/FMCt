module FMCt.Pretty (
    printStack,
    printOutput,
) where

import Data.Map as M
import FMCt.Evaluator
import FMCt.Syntax

printStack :: State -> String
printStack (m, b) = "Memory: \n" ++ printer mem ++ "Bindings: \n" ++ printer bin
  where
    mem = M.toList m
    bin = M.toList b
    printer [] = ""
    printer ((l, ts) : xs) = show l ++ "[" ++ show ts ++ "]" ++ "\n" ++ printer xs

printOutput :: State -> String
printOutput (m, _) = "Output: \n" ++ printer out
  where
    out = maybe [] reverse (m M.!? Out)
    printer = \case
        [] -> ""
        (x : xs) -> show x ++ "\n" ++ printer xs
