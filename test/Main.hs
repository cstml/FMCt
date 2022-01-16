module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

double x = x * 2

half x = x `div` 2

main = defaultMain unitTests

unitTests =
    testGroup
        "Unit tests"
        [doublingMakesNumbersBigger, halvingMakesNumbersSmaller]

doublingMakesNumbersBigger =
    testCase "Double of 4 is 8" $ assertEqual [] 8 (double 4)

halvingMakesNumbersSmaller =
    testCase "Half of 9 is 4" $ assertEqual [] 4 (half 9)
