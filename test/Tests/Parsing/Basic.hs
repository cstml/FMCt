module Tests.Parsing.Basic (parsingTests) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import FMCt

parsingTests :: TestTree
parsingTests = testGroup "All Parsing Tests"
  [ parseVariable
  , inferredTypes
  , parsePush
  ]

parseVariable =                                              
  let
    tStr str repr = testCase str $ assertEqual str (pure repr) (parseFMC' str)
  in
    testGroup "Variable Parsing Tests"
      [ tStr "x;*"  (V "x" St)
      , tStr "    x    "   (V "x" St)
      , tStr " +" (V "+" St)
      , tStr "x;x" (V "x" $ V "x" St)
      , tStr "x;x" (V "x" $ V "x" St)
      , tStr "x;X" (V "x" $ V "X" St)
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
{-
failPush =
  let
    tFPush str err = testCase str $ assertEqual str (Left . pure $  err) (parseFMC' str)
  in
  testGroup "Failed Push Terms"
  [
    tFPush "[x" "FMCParser"
  ]
-}

inferredTypes :: TestTree
inferredTypes =
  let
    tStr str repr = testCase "Inferred Term" $ assertEqual str (pure repr) (parseFMC' str)
  in
    testGroup "Inferred Application/Pop Terms;"
    [ tStr "in<x>;*" (B "x" (TVar "_") In St)
    , tStr "in<x>" (B "x" (TVar "_") In St)
    ]
