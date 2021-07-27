module FMCt.Examples
  (examplesList)
where
import FMCt.Parsing (parseFMC) 
import FMCt.TypeChecker (derive) 

examplesList = [term1]
  where
  term1 :: String 
  term1 =  "1.2.3.γ<x:(=>^(Int))>.+.x.+"


ex04   = derive $ parseFMC "*"
ex14   = derive $ parseFMC "x.y.*"
ex24   = derive $ parseFMC "<x:(=>a)>.*"
ex34   = derive $ parseFMC "<x:a>.x.*"
ex34'  = derive $ parseFMC "[*].<x:(=>)>.[x.*].*" -- First ever working term
ex34'' = derive $ parseFMC "[*].<x:(=>)>.[x.*].<y:(=>)>.*" -- First ever working term
ex44   = derive $ parseFMC "<x:a>.<y:a>.x.*"
ex54   = derive $ parseFMC "[x.*].*"
ex64   = derive $ parseFMC "[<x:a>.x.*].*"
ex74   = derive $ parseFMC "[<x:(=>a)>.x.*].<y:a>.*"
ex84   = derive $ parseFMC "[*].*"
ex94   = derive $ parseFMC "[*].<x:(=>)>.*"
