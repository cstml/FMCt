module FMCt.Examples
  (examplesList)
where
import FMCt.Parsing (parseFMC) 
import FMCt.TypeChecker (derive, fuse, consumes, consume)
import FMCt.Syntax (T, Type(..), Lo(..))

examplesList = [term1]
  where
  term1 :: String 
  term1 =  "1.2.3.γ<x:(=>^(Int))>.+.x.+"

ex04   = derive $ parseFMC "*"
ex14   = derive $ parseFMC "x.y.*"
ex24   = derive $ parseFMC "<x:(=>a)>.*"
ex34   = derive $ parseFMC "[*].<x:(=>)>.x.x.x.*"
ex34'  = derive $ parseFMC "[*].<x:(=>)>.[x.*].*" -- First ever working term
ex34'' = derive $ parseFMC "[*].<x:(=>)>.[x.*].<y:(=>)>.*" -- First ever working term
ex44   = derive $ parseFMC "<x:a>.<y:a>.x.*"
ex54   = derive $ parseFMC "[x.*].*"
ex64   = derive $ parseFMC "[<x:a>.x.*].*"
ex74   = derive $ parseFMC "[<x:(=>a)>.x.*].<y:a>.*"
ex84   = derive $ parseFMC "[*].*"
ex94   = derive $ parseFMC "[*].<x:(=>)>.*"

--------------------------------------------------------------------------------
-- Fuse Examples

exFuse1 = (TCon "") `fuse` (TCon "x")
exFuse2 = (TCon "y" :=> TCon "x") `fuse` (TCon "x" :=> TCon "y")
exFuse3 = (TCon "y" :=> TVec[TCon "x", TLoc Ho $ TCon ""]) `fuse` (TCon "x" :=> TCon "y")

--------------------------------------------------------------------------------
-- consumes examples

xc1 = consumes (TCon "x") (TCon "x")
xc2 = consumes (TVec [ TCon "y", TCon "x", TCon "z" ]) (TCon "y")
-- | One extra Received
xc3 = consumes (TVec [ TCon "y", TCon "x", TCon "z" ])
      (TVec [ TCon "y", TCon "x", TCon "z", TCon "p" ])
-- | One extra Required
xc4 = consumes
      (TVec [ TCon "y", TCon "x", TCon "z", TCon "p" ])
      (TVec [ TCon "y", TCon "x", TCon "z" ])
xc5 = consumes (TVec [ TCon "y" ]) (TVec [ TLoc Ho $ TCon "y" ])

-- | The inputs at the locations do interact.
xc6 = consumes (TVec [ TLoc Ho $ TCon "y" ]) (TVec [ TLoc Ho $ TCon "y" ])

-- | The inputs at the locations do interact but leave some behind.
xc7 = consumes (TVec [ TCon "x", TLoc Ho $ TCon "y" ]) (TVec [ TLoc Ho $ TCon "y" ])

-- | This should saturate, even though the inputs are reversed
xc8 = consumes (TVec [ TCon "x", TLoc Ho $ TCon "y" ]) (TVec [ TLoc Ho $ TCon "y", TCon "x" ])

exCons :: Either String T
-- | Simple fusion example.
exCons = (TCon "a" :=> TCon "b") `consume` (TCon "b" :=> TCon "c")
-- | This Time wrapped in a vector.
exCons' = (TCon "a" :=> TVec[TCon "b"]) `consume` (TCon "b" :=> TCon "c")
-- | Wrapped in two vectors.
exCons'' = (TCon "a" :=> TVec[TVec[TCon "b"]]) `consume` (TCon "b" :=> TCon "c")
exCons2 = (TCon "a" :=> TCon "b") `consume` (TCon "" :=> TCon "c")
