module Tests.Parsing.Basic (parsingTests) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import FMCt

parsingTests :: TestTree
parsingTests =
  let
    tStr str repr = testCase "Succesfully Parse Term" $ assertEqual str (pure repr) (parseFMC' str)
  in
    testGroup "Parsing Tests"
      [ tStr "x;*\n"  (V "x" St)
      , tStr "x;x" (V "x" $ V "x" St)
      , tStr "x.x" (V "x" $ V "x" St)
      , tStr "x.X" (V "x" $ V "X" St)
      , inferredTypes
      ]

inferredTypes :: TestTree
inferredTypes =
  let
    tStr str repr = testCase "Inferred Term" $ assertEqual str (pure repr) (parseFMC' str)
  in
    testGroup "Inferred Application/Pop Terms;"
    [ tStr "in<x>;*" (B "x" (TVar "_") In St)
    , tStr "in<x>" (B "x" (TVar "_") In St)
    , tStr "in<x> ; out<y>" (B "x" (TVar "_") In (B "y" (TVar "_") Out St))
    ]
