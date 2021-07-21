module FMCt where

-- Module is made just for Re-Exporting basic functions.
import FMCt.Parsing    ( parseFMC, parseType, parseFMCtoString)
import FMCt.Evaluator  ( eval )
import FMCt.Examples   ( examplesList )
import FMCt.TypeChecker( typeCheck )
