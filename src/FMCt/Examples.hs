module FMCt.Examples (
    examplesList,
) where

-- | A useful list of examples, ready to be parsed.
examplesList :: [String]
examplesList =
  [ "[1.2.3]γ.γ<x:a=>(Int,Int,Int))>.x.+"
  , "-"
  , "/"
  , "1.2./"
  , "x.y.*"
  , "<x:()=>a>"
  , "[[]].<x:_>.x.x.x"
  , "[].<x:()=>Int>.[x].*" -- First ever working term
  , "[]la.<x:a=>b>.[x].<y:a=>b>.*" 
  , "<x:_>.<y:_>.x.*"
  , "[[1]].*"
  , "[<x:_>.x].True"
  , "[<x:a=>a>.x.*].<y:a=>()>"
  , "[*].*"
  ]
