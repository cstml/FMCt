module TestGenerators where

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
import Generators.Term
import Generators.Type
import Test.QuickCheck
