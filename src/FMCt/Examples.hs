{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}

module FMCt.Examples (
    examplesList,
--    pEx,
--    runPEx,
) where

import FMCt.Evaluator
import FMCt.Parsing (parseFMC)
import FMCt.Syntax (Lo (..), T, Tm (..), Type (..))
import FMCt.TypeChecker (TError, consume, consumes, derive, fuse, getTermType, (<.>))

examplesList :: [String]
examplesList = [term1]
  where
    term1 :: String
    term1 = "1.2.3.γ<x:(=>^(Int))>.+.x.+"
{-
ex04 = derive $ parseFMC "*"

ex14 = derive $ parseFMC "x.y.*"

ex24 = derive $ parseFMC "<x:(=>a)>.*"

ex34 = derive $ parseFMC "[*].<x:(=>)>.x.x.x.*"

ex34' = derive $ parseFMC "[*].<x:(=>)>.[x.*].*" -- First ever working term

ex34'' = derive $ parseFMC "[*].<x:(=>)>.[x.*].<y:(=>)>.*" -- First ever working term

ex44 = derive $ parseFMC "<x:a>.<y:a>.x.*"

ex54 = derive $ parseFMC "[x.*].*"

ex64 = derive $ parseFMC "[<x:a>.x.*].*"

ex74 = derive $ parseFMC "[<x:(=>a)>.x.*].<y:a>.*"

ex84 = derive $ parseFMC "[*].*"

ex94 = derive $ parseFMC "[*].<x:(=>)>.*"

--------------------------------------------------------------------------------
-- Fuse Examples

exFuse1 = (TCon "") `fuse` (TCon "x")

exFuse2 = (TCon "x") `fuse` (TCon "")

exFuse3 = (TCon "y" :=> TCon "x") `fuse` (TCon "x" :=> TCon "y")

exFuse4 = (TCon "y" :=> TVec [TCon "x", TLoc Ho $ TCon ""]) `fuse` (TCon "x" :=> TCon "y")

exFuse5 = (mempty :=> TCon "a") `fuse` (mempty :=> TCon "a")

exFuse6 = (mempty :=> TVec [TCon "Int", TLoc La (TCon "Int")]) `fuse` (TLoc La (TCon "Int") :=> mempty)

--------------------------------------------------------------------------------
-- consumes examples

xc1 = consumes (TCon "x") (TCon "x")

xc2 = consumes (TVec [TCon "y", TCon "x", TCon "z"]) (TCon "y")

-- | One extra Received
xc3 =
    consumes
        (TVec [TCon "y", TCon "x", TCon "z"])
        (TVec [TCon "y", TCon "x", TCon "z", TCon "p"])

-- | One extra Required
xc4 =
    consumes
        (TVec [TCon "y", TCon "x", TCon "z", TCon "p"])
        (TVec [TCon "y", TCon "x", TCon "z"])

xc5 = consumes (TVec [TCon "y"]) (TVec [TLoc Ho $ TCon "y"])

-- | The inputs at the locations do interact.
xc6 = consumes (TVec [TLoc Ho $ TCon "y"]) (TVec [TLoc Ho $ TCon "y"])

-- | The inputs at the locations do interact but leave some behind.
xc7 = consumes (TVec [TCon "x", TLoc Ho $ TCon "y"]) (TVec [TLoc Ho $ TCon "y"])

-- | This should saturate, even though the inputs are reversed
xc8 = consumes (TVec [TCon "x", TLoc Ho $ TCon "y"]) (TVec [TLoc Ho $ TCon "y", TCon "x"])

xc9 = consumes (TLoc Ho $ TCon "a") (TLoc Ho $ TCon "b")

--------------------------------------------------------------------------------
-- Fusion Examples
fEx1 = derive . parseFMC $ "1.2.+"

--------------------------------------------------------------------------------
-- Cons example
exCons :: Either TError T

-- | Simple fusion example.
exCons = (TCon "a" :=> TCon "b") `consume` (TCon "b" :=> TCon "c")

-- | This Time wrapped in a vector.
exCons' = (TCon "a" :=> TVec [TCon "b"]) `consume` (TCon "b" :=> TCon "c")

-- | Wrapped in two vectors.
exCons'' = (TCon "a" :=> TVec [TVec [TCon "b"]]) `consume` (TCon "b" :=> TCon "c")

exCons2 = (TCon "a" :=> TCon "b") `consume` (TCon "" :=> TCon "c")

--------------------------------------------------------------------------------
-- parse example
runPEx = parseFMC <$> pEx

pEx = [pEx1, pEx2, pEx3, pEx4, pEx5]
  where
    pEx1 = "x . y . [*]. [*] . <x:((int,bool))>"
    pEx2 = "x . y . [*]. [*] . <x:_>"
    pEx3 = "x . y . [*]. [*] . <x:(a(int,bool) => (int))>"
    pEx4 = "x . y . [*]. [*] . <x:(a(a) => (a) => (b))>" -- higher order type
    pEx5 = "x . y . [*]. [*] . <x:(a(ab,b) => (int)), (b(int))>"

--------------------------------------------------------------------------------
-- Some simple Evaluator examples
ex7 =
    eval1 $ -- [1.2.*].<x:t>.x.3.4
        P
            (V "1" $ V "2" St)
            La -- [1 . 2 . *]
            ( B "x" (TVec [] :=> TVec [TLoc Ho $ TCon "a"]) La $ -- <x:t>
                V "x" $ -- x
                    V "3" $ -- 3
                        V "4" $ -- 4
                            V "+" $ -- +
                                V
                                    "+" -- +
                                    St -- Star
            )

ex8 =
    eval1 -- 1 . 2 . <x:t>_ . x . +
        ( V "1" $ -- 1
            V "2" $ -- 2
                B "x" (TVec [TLoc Ho $ TCon "a"]) Ho $ -- <x>
                    V "x" $ -- x
                        V "+" St -- +
        )
-}
