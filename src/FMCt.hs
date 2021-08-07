-- | Module is made just for Re-Exporting basic functions.
module FMCt (module X) where

import FMCt.Evaluator as X (eval, eval1, eval1', evalToString)
import FMCt.Examples as X (examplesList)
import FMCt.Parsing as X (parseFMC, parseFMCtoString, parseType)
import FMCt.Pretty as X (printOutput, printStack)
import FMCt.TypeChecker as X (derive, typeCheck, typeCheckP)
