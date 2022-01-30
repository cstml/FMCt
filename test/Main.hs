module Test.Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Tests.Parsing.Basic

double x = x * 2

half x = x `div` 2

main = defaultMain unitTests

parsingTest :: TestTree
parsingTest =
  let
    tStr str repr = testCase "Succesfully Parse Term" $ assertEqual str (parseFMC' str) (pure repr)
  in
    testGroup "Parsing Tests"
      [ tStr "x"  (V "x" St)
      , tStr "x;x" (V "x" $ V "x" St)
      , tStr "x.x" (V "x" $ V "x" St)
      , tStr "x.X" (V "x" $ V "X" St)
      ]

unitTests = 
    testGroup
        "Unit tests"
        [ doublingMakesNumbersBigger
        , halvingMakesNumbersSmaller
        , parsingTest
        ]

doublingMakesNumbersBigger =
    testCase "Double of 4 is 8" $ assertEqual [] 8 (double 4)

halvingMakesNumbersSmaller =
    testCase "Half of 9 is 4" $ assertEqual [] 4 (half 9)
