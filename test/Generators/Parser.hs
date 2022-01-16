module Generators.Parse where

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
import Test.QuickCheck

-- all terms are equal to themselves
pRefl :: Property
pRefl = forAll genTerm (\x -> x === x)

-- test it
pReflTest :: IO ()
pReflTest = quickCheck pRefl

genTerm :: Gen String
genTerm = oneof [genPush, genStar, genVar, genPop]

genStar :: Gen String
genStar = pure "*"

genVar :: Gen String
genVar = do
    v <- genString
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
                    [ "[",
                      t,
                      "]",
                      lo,
                      ";",
                      l
                    ]

genPop :: Gen String
genPop = do
    t <- genString
    lo <- genLoc
    l <- genTerm
    pure $ mconcat [lo, "<", t, ">", ";", l]

genLoc :: Gen String
genLoc = genString
