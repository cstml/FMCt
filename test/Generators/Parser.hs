module Generators.Parser where

import Control.Monad (forM_, void)
import Control.Monad.ST
import qualified Data.Map as M
import Data.String (IsString (..))
import FMCt.Evaluator
import FMCt.Examples
import FMCt.Parsing
import FMCt.Pretty
import FMCt.Syntax
import FMCt.TypeChecker
import Generators.Instances
import Test.QuickCheck

-- | Parser Tester: the type and its underlying parse value
newtype PT a = PT {getRepr :: (a, String)}

instance Arbitrary (PT T) where
  arbitrary = (\t -> PT (t, pShow t)) <$> arbitrary

instance Arbitrary (PT Lo) where
  arbitrary = (\x -> PT (Lo x, x)) <$> arbitrary

genTerm :: Gen String
genTerm = oneof [genPush, genStar, genVar, genPop]

genStar :: Gen String
genStar = pure "*"

genVar :: Gen String
genVar = do
  v <- arbitrary
  if v == "*"
    then return v
    else do
      c <- genTerm
      return $ mconcat [v, ";", c]

genPush :: Gen String
genPush = do
  t <- genTerm
  lo <- genLoc
  if t == "*"
    then pure t
    else do
      l <- genTerm
      pure $
        mconcat
          [ "["
          , t
          , "]"
          , lo
          , ";"
          , l
          ]

genPop :: Gen String
genPop = do
  t <- arbitrary
  lo <- genLoc
  l <- genTerm
  pure $ mconcat [lo, "<", t, ">", ";", l]

genLoc :: Gen String
genLoc = arbitrary
