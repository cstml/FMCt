module Tests.Parsing.Basic (parsingTests) where

import FMCt
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

parsingTests :: TestTree
parsingTests =
  testGroup
    "All Parsing Tests"
    [ parseVariable
    , inferredTypes
    , parsePush
    , parsePop
    ]
 where
  parseVariable =
    let tStr str repr = testCase str $ assertEqual str (pure repr) (parseFMC str)
     in testGroup "Variable Parsing Tests" $
          uncurry tStr
            <$> [
                  ( "x    "
                  , V "x" St
                  )
                ,
                  ( " y;*    "
                  , V "y" St
                  )
                ,
                  ( " *    "
                  , St
                  )
                ,
                  ( "x ; x;"
                  , V "x" $ V "x" St
                  )
                ,
                  ( "    x;  "
                  , V "x" St
                  )
                ,
                  ( " +"
                  , V "+" St
                  )
                ,
                  ( "x;x"
                  , V "x" $ V "x" St
                  )
                ,
                  ( "x;x"
                  , V "x" $ V "x" St
                  )
                ,
                  ( "x;X"
                  , V "x" $ V "X" St
                  )
                ,
                  ( "*"
                  , St
                  )
                ,
                  ( ""
                  , St
                  )
                ]

  parsePush =
    let tPush str repr = testCase str $ assertEqual str (pure repr) (parseFMC str)
     in testGroup "Push Terms" $
          uncurry tPush
            <$> [
                  ( "[x]@in"
                  , P (V "x" St) In St
                  )
                ,
                  ( "[[x]]"
                  , P (P (V "x" St) La St) La St
                  )
                ,
                  ( "[[*]]"
                  , P (P St La St) La St
                  )
                ,
                  ( "[x];[y]"
                  , P (V "x" St) La $ P (V "y" St) La St
                  )
                ,
                  ( "[[]]"
                  , P (P St La St) La St
                  )
                ,
                  ( "[[]@in]@out"
                  , P (P St In St) Out St
                  )
                ]

  parsePop =
    let tPop str repr = testCase str $ assertEqual str (pure repr) (parseFMC str)
     in testGroup "Pop Term" $
          uncurry tPop
            <$> [
                  ( "<x>"
                  , B "x" (TVar "_") La St
                  )
                ,
                  ( "@in<x> ; <x>"
                  , B "x" (TVar "_") In $ B "x" (TVar "_") La St
                  )
                ,
                  ( "@in<x> ; <y:_>"
                  , B "x" (TVar "_") In $ B "y" (TVar "_") La St
                  )
                ,
                  ( "@in<x> ; <y:_> ; [<z>;*] ;*"
                  , B "x" (TVar "_") In $ B "y" (TVar "_") La $ P (B "z" (TVar "_") La St) La St
                  )
                ]

  inferredTypes =
    let tStr str repr = testCase "Inferred Term" $ assertEqual str (pure repr) (parseFMC str)
     in testGroup "Inferred Application/Pop Terms;" $
          uncurry tStr
            <$> [
                  ( "@in<x>;*"
                  , B "x" (TVar "_") In St
                  )
                ,
                  ( "@in<x>"
                  , B "x" (TVar "_") In St
                  )
                ]
