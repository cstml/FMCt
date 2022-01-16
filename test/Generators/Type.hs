module Generators.Type where

import FMCt.Syntax
import FMCt.TypeChecker
import Generators.Aux
import Test.QuickCheck (Gen, arbitrary, elements, listOf, oneof, resize, sample, sized, vectorOf)

------------------------------------------------------------------------------
-- Type Generators

-- | Type Generator
genType :: Gen T
genType = oneof [genTypeConstant, genLocationType, genHigherType]

-- | Constant Type Generator
genTypeConstant :: Gen T
genTypeConstant = TCon <$> genConst

-- | Location Type Generator
genLocationType :: Gen T
genLocationType = TLoc <$> genLocation <*> genType

-- | Higher Type Generator
genHigherType :: Gen T
genHigherType = (:=>) <$> genType <*> genType

genConst :: Gen String
genConst = vectorOf 3 (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])
