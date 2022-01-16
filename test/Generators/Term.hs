module Generators.Term where

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
import Generators.Type
import Test.QuickCheck

--------------------------------------------------------------------------------
-- Term Generators

-- | Binder Generator
genBinder :: Gen String
genBinder = vectorOf 3 (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

-- | Location Generator
genLocation = oneof [genPreDefLocation, genRandLocation]

-- | Predefined Location Generator
genPreDefLocation :: Gen Lo
genPreDefLocation = elements [Out, In, Rnd, Nd, Ho, La]

-- | Random Location Generator
genRandLocation :: Gen Lo
genRandLocation = Lo <$> genBinder

--------------------------------------------------------------------------------
-- Term Generators

-- | Term Generator
genTerm :: Gen Tm
genTerm = oneof [genVariables, genStar, genApp, genAbs]

-- | Variable Generator
genVariables :: Gen Tm
genVariables = do
    b <- genBinder
    t <- genTerm
    return $ V b t

-- | Application Generator
genApp :: Gen Tm
genApp = do
    t1 <- genTerm
    t2 <- genTerm
    l <- genLocation
    return $ P t1 l t2

genAbs :: Gen Tm
genAbs = do
    b <- genBinder
    t <- genType
    l <- genLocation
    t' <- genTerm
    return $ B b t l t'

-- | Star Generator
genStar :: Gen Tm
genStar = return St
