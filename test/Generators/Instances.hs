module Generators.Instances where

import Control.Lens
import FMCt.Evaluator
import FMCt.Examples
import FMCt.Parsing
import FMCt.Pretty
import FMCt.Syntax
import FMCt.TypeChecker
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary TConstant where
  arbitrary = do
    f <- choose ('A', 'Z')
    g <- choose ('a', 'z')
    pure . review tConstant $ [f, g]

instance Arbitrary TVariable where
  arbitrary = do
    f <- choose ('a', 'z')
    g <- choose ('A', 'Z')
    pure . review tVariable $ [f, g]

instance Arbitrary T where
  arbitrary =
    oneof
      [ TCon <$> arbitrary
      , TVar <$> arbitrary
      , TVec . getNonEmpty <$> arbitrary
      , TLoc <$> arbitrary <*> arbitrary
      , (:=>) <$> arbitrary <*> arbitrary
      , pure TEmp
      ]
  shrink x = case x of
    TVec (x : xs) -> []
    _ -> []

instance Arbitrary Tm where
  arbitrary = gTerm
   where
    gTerm =
      oneof
        [ pure St
        , V <$> arbitrary <*> arbitrary
        , P <$> arbitrary <*> arbitrary <*> arbitrary
        , B <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary Lo where
  arbitrary = gLo
   where
    gLo =
      oneof $
        ( do
            f <- choose ('a', 'z')
            g <- choose ('A', 'Z')
            pure . Lo $ [f, g]
        ) :
        (pure <$> [Out, In, Rnd, Nd, Ho, La])
