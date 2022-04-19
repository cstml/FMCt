module Tests.Evaluator.Basic (evaluateTest) where

import Control.Lens
import FMCt
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

evaluateTest :: TestTree
evaluateTest =
  testGroup
    "All Evaluator Tests"
    [ star
    ]

star :: TestTree
star =
  let t term res =
        let pTerm = parseFMC term
         in testCase (show $ pShow <$> pTerm) $ assertEqual term (eval <$> pTerm) (Right res)
   in testGroup "Trivial Star Evaluation" $
        uncurry t
          <$> [ ("*", mempty)
              , ("[*]@a;*", memory . at (Lo "a") ?~ [St] $ mempty)
              , ("[*];*", memory . at La ?~ [St] $ mempty)
              ]
