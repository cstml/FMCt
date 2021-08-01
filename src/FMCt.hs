-- | Module is made just for Re-Exporting basic functions.
module FMCt
  (  module X  )
where

import FMCt.Parsing     as X ( parseFMC, parseType, parseFMCtoString )
import FMCt.Evaluator   as X ( eval, eval1, eval1', tCheck', evalToString ) 
import FMCt.Examples    as X ( examplesList )
import FMCt.TypeChecker as X ( typeCheck, derive, typeCheckP)
import FMCt.Pretty      as X ( printStack, printOutput)
