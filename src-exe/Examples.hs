module Examples where

import Parsing
import Evaluator
import Syntax
import Pretty
import TypeChecker

--  Types

t0 :: T
t0 = TLocat La (TLocat La (TConst "")) :=> (TLocat La (TConst ""))
     
t1 :: T
t1 = TVector [ t0, t0 ]

equalityT = t0 == t1

-- Terms
term1 :: String 
term1 =  "1.2.3.γ<x:(=>^(Int))>.+.x.+"

