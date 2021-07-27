-- | Module is made just for Re-Exporting basic functions.
module FMCt
  (  module X  )
where
import qualified FMCt.Parsing     as X ( parseFMC, parseType, parseFMCtoString )
import qualified FMCt.Evaluator   as X ( eval ) 
import qualified FMCt.Examples    as X ( examplesList )
import qualified FMCt.TypeChecker as X ( typeCheck )
