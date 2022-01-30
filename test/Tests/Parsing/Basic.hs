module Tests.Parsing.Basic where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import FMCt

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
