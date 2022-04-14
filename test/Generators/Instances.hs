module Generators.Instances where

import FMCt.Evaluator
import FMCt.Examples
import FMCt.Parsing
import FMCt.Pretty
import FMCt.Syntax
import FMCt.TypeChecker

import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Gen

instance Arbitrary T where
  arbitrary = gT
    where
      nonEmptyS = (getNonEmpty) <$> (arbitrary :: Gen (NonEmptyList Char))
      gT = oneof
        [ TCon <$> nonEmptyS
        , TVar <$> nonEmptyS
        , (TVec . getNonEmpty) <$> arbitrary
        , TLoc <$> arbitrary <*> arbitrary
        , (:=>) <$> arbitrary <*> arbitrary
        , pure TEmp
        ]
      

instance Arbitrary Tm where 
  arbitrary = gTerm
    where
      gTerm = oneof
        [ pure St 
        , V <$> arbitrary <*> arbitrary       
        , P <$> arbitrary <*> arbitrary <*> arbitrary
        , B <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary Lo where
  arbitrary = gLo
    where
      gLo = oneof $
         (Lo <$> arbitrary) : (pure <$> [ Out, In, Rnd, Nd, Ho, La ])
