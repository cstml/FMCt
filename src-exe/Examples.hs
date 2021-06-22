module Examples where

import Evaluator
import Syntax
import Pretty
import TypeChecker

--  Types

t0 :: T
t0 = [ K $ T La []
     , K ( T La [] ) :=> K (T La []) :=> K (T La [])
     ]

t1 :: T
t1 = [ K $ T La []
     , K ( T La ["a"] ) :=> K ( T La ["a"] ) :=> K ( T La ["a"] )
     ]


t2 :: T
t2 = [ K $ T La []
     , K ( T La ["a"] ) :=> K ( T La ["a"] ) :=> K ( T La ["a"] )
     , K ( T La ["a"] ) :=> K ( T La ["a"] ) :=> K ( T La ["a"] )
     ]

equalityT = t1 == t2

-- Terms
term1 :: String 
term1 =  "1.2.3.γ<x:_>.+.x.+"
