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

import Generators.Parser
import Generators.Instances

import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty
import Test.QuickCheck.Gen

testProps :: TestTree
testProps = testGroup "All Props" [ evaluatorTests, parserTests ]

evaluatorTests :: TestTree
evaluatorTests = testProperty "Trivial gStar" reflStar

parserTests :: TestTree
parserTests = testProperties "Parser Tests" [("Total Type Parser:", parseTotalT)] 

parseTotalT :: Property
parseTotalT = forAll (arbitrary) (\t -> t === (parseType . pShow) t)

{- For all the terms generated by gStar they are all Star. -}
reflStar :: Property
reflStar = forAll arbitrary (\p -> p == St ==> St === p )
        
      
