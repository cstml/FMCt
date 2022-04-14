module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Tests.Parsing.Basic
import Tests.Evaluator.Basic (evaluateTest)
import TestGenerators 

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests:" [ parsingTests, evaluateTest ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests: " [ testProps ]
