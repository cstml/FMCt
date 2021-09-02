-- | Module is made just for Re-Exporting basic functions.
module FMCt (module X, module XX, module Y) where

import FMCt.Aux.Pretty     as X
import FMCt.Aux.ToTex      as X
import FMCt.Evaluator      as X  (eval, eval1, eval1', evalToString)
import FMCt.Examples       as X  (examplesList)
import FMCt.Parsing        as X  (parseFMC, parseFMCtoString, parseType)
import FMCt.Pretty         as X  (printOutput, printStack)
import FMCt.TypeChecker    as X  (derive, typeCheck, typeCheckP)
import FMCt.TypeChecker2   as XX (derive2, getTermType, pShow', normalForm, testD2)
import FMCt.Syntax         as X  (T, Type(..),)
import FMCt.TypeCheckerAlt as Y  (derive1, testD1)
