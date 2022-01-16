module FMCt.Pretty
    ( printStack,
      printSubs,
    )
where

import Control.Lens
import Data.Map as M
import FMCt.Evaluator
import FMCt.Syntax
import FMCt.TypeChecker

printStack :: EvalState -> String
printStack s = mconcat ["Memory: \n", printer m, "Bindings: \n", printer b]
    where
        m = M.toList $ s ^. memory
        b = M.toList $ s ^. binds
        printer [] = ""
        printer ((l, ts) : xs) = show l ++ "[" ++ show ts ++ "]" ++ "\n" ++ printer xs

-- | Pretty Prints all the bindings in a context for derivation from TypeChecker 2.
printBindings2 :: Derivation -> String
printBindings2 = auxCtxPP . getContext

-- | Shared Pretty Printer.
auxCtxPP :: [(String, T)] -> String
auxCtxPP = mconcat . fmap (\(b, t) -> mconcat [" ", b, " : ", pShow t, "\n"])

-- | Pretty Prints substitutions
printSubs :: [TSubs] -> String
printSubs = mconcat . fmap (\(x, y) -> mconcat [" ", pShow x, " ~> ", pShow y, "\n"])
