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
    , simpleBinds
    ]
  where
    t term res =
      let pTerm = parseFMC term
      in testCase (show $ pShow <$> pTerm) $ assertEqual term (eval <$> pTerm) (Right res)
      
    star =
      testGroup "Trivial Star Evaluation" $ (uncurry t)
          <$> [ ("*", mempty)
              , ("[*]@a;*", memory . at (Lo "a") ?~ [St] $ mempty)
              , ("[*];*", mempty & memory . at La ?~ [St] )
              ]
      
    simpleBinds =
      testGroup "Simple Binds" $
      (uncurry t) <$>
      [ ( "[x];<y>"
        , mempty &
          ( (binds . at "y" ?~  V "x" St)
            . (memory . at La ?~ [])
          )
        )
      , ( "[x]@x;@x<y>"
        , mempty &
          ( (binds . at "y" ?~  V "x" St)
            . (memory . at (Lo "x") ?~ [])
          )
        )     
      ]
          
    
