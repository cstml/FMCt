module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestGenerators
import Tests.Evaluator.Basic (evaluateTest)
import Tests.Parsing.Basic

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All Tests" [unitTests, propertyTests]
 where
  unitTests = testGroup "Unit Tests:" [parsingTests, evaluateTest]
  propertyTests = testGroup "Property Tests: " [testProps]
