module Generators.Aux where

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

--------------------------------------------------------------------------------
-- Aux

genString :: Gen String
genString =
    let l = elements ['a' .. 'z']
     in do
            x <- l
            y <- l
            return [x, y]

-- | Location Generator
genLocation = oneof [genPreDefLocation, genRandLocation]

-- | Predefined Location Generator
genPreDefLocation :: Gen Lo
genPreDefLocation = elements [Out, In, Rnd, Nd, Ho, La]

-- | Random Location Generator
genRandLocation :: Gen Lo
genRandLocation = Lo <$> genString
