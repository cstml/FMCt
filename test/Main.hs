module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Tests.Parsing.Basic
import Tests.Evaluator.Basic (evaluateTest)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = 
    testGroup
        "Unit tests"
        [ parsingTests
        , evaluateTest
        ]
