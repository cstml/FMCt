module FMCt.Pretty (
    printStack,
    printOutput,
    printBindings2,
    printBindingsA,
    printSubs,
) where

import Data.Map as M
import FMCt.Evaluator
import FMCt.Syntax
import qualified FMCt.TypeChecker2 as T2
import qualified FMCt.TypeCheckerAlt as TA

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

-- | Pretty Prints all the bindings in a context for derivation from TypeChecker 2.
printBindings2 :: T2.Derivation -> String
printBindings2 = auxCtxPP . T2.getContext

-- | Pretty Prints all the bindings in a context for derivation from TypeChecker Alt.
printBindingsA :: TA.Derivation -> String 
printBindingsA = auxCtxPP . TA.getContext

-- | Shared Pretty Printer.
auxCtxPP :: [(String, T)] -> [Char]
auxCtxPP = mconcat . fmap (\(b,t) -> mconcat [ " ", b, " : ", pShow t, "\n"])

-- | Pretty Prints substitutions
printSubs :: [T2.TSubs] -> String
printSubs = mconcat . fmap (\(x,y) -> mconcat [" ", pShow x, " ~> ", pShow y, "\n"])
