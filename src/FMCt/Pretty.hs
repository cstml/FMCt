module FMCt.Pretty (
    printStack,
    printSubs,
) where

import FMCt.TypeChecker.Aux (sTo, sFrom)
import Control.Lens
import Data.Map as Map
import FMCt.Evaluator
import FMCt.Syntax
import FMCt.TypeChecker
import Control.Arrow ((>>>))
import FMCt.TypeChecker.Aux

printStack :: EvalState -> String
printStack s = mconcat ["Memory: \n", printer m, "Bindings: \n", printer b]
  where
    m = Map.toList $ s ^. memory
    b = Map.toList $ s ^. binds
    printer [] = ""
    printer ((l, ts) : xs) = show l ++ "[" ++ show ts ++ "]" ++ "\n" ++ printer xs

-- | Pretty Prints all the bindings in a context for derivation from TypeChecker 2.
printBindings2 :: Derivation -> String
printBindings2 = view judgement >>> auxCtxPP 

-- | Shared Pretty Printer.
auxCtxPP :: Judgement -> String
auxCtxPP =
  view jContext
  >>> Map.toList
  >>> fmap (\(b, t) -> mconcat [" ", b, " : ", pShow t, "\n"])
  >>> mconcat

-- | Pretty Prints substitutions
printSubs :: [TSubs] -> String
printSubs =
  fmap (\a -> let x = a ^. sFrom ; y = a ^. sTo in  mconcat [" ", pShow x, " ~> ", pShow y, "\n"])
  >>> mconcat
