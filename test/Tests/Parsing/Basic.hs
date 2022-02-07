module Tests.Parsing.Basic (parsingTests) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import FMCt

parsingTests :: TestTree
parsingTests = testGroup "All Parsing Tests"
  [ parseVariable
  , inferredTypes
  , parsePush
  , parsePop
  ]

parseVariable =
  let
    tStr str repr = testCase str $ assertEqual str (pure repr) (parseFMC' str)
  in
    testGroup "Variable Parsing Tests"
      [ tStr "x    "  $ V "x" St
      , tStr " y;*    "  $ V "y" St
      , tStr " *    "  St
      , tStr "x ; x;" $ V "x" $ V "x" St
      , tStr "    x;  "   $ V "x" St
      , tStr " +"  $ V "+" St 
      , tStr "x;x" $ V "x" $ V "x" St
      , tStr "x;x" $ V "x" $ V "x" St
      , tStr "x;X" $ V "x" $ V "X" St
      , tStr "*" St
      , tStr "" St
      ]

parsePush =
  let
    tPush str repr = testCase str $ assertEqual str (pure repr) (parseFMC' str)
  in
    testGroup "Push Terms"
    [ tPush "[x]in"   $ P (V "x" St) In St
    , tPush "[[x]]"   $ P (P (V "x" St) La St) La St
    , tPush "[x];[y]" $ P (V "x" St) La  $ P (V "y" St) La $ St
    , tPush "[[]]"    $ P (P St La St) La St
    , tPush "[[]in]out" $ P (P St In St) Out St
    ]

parsePop =
  let
    tPop str repr = testCase str $ assertEqual str (pure repr) (parseFMC' str)
  in
    testGroup "Pop Term" 
    [ tPop "<x>" $ B ("x") (TVar "_") La St
    , tPop "in<x> ; <x>" $ B "x" (TVar "_") In $ B "x" (TVar "_") La St
    , tPop "in<x> ; <y:_>" $ B "x" (TVar "_") In $ B "y" (TVar "_") La St
    , tPop "in<x> ; <y:_> ; [<z>;*] ;*" $ B "x" (TVar "_") In $ B "y" (TVar "_") La $ P (B "z" (TVar "_") La St ) La St
    ]

inferredTypes :: TestTree
inferredTypes =
  let
    tStr str repr = testCase "Inferred Term" $ assertEqual str (pure repr) (parseFMC' str)
  in
    testGroup "Inferred Application/Pop Terms;"
    [ tStr "in<x>;*" (B "x" (TVar "_") In St)
    , tStr "in<x>" (B "x" (TVar "_") In St)
    ]
