module TestGenerators
  ( genTerm
  , genType
  )
where
import FMCt.Syntax
import FMCt.Parsing
import FMCt.Pretty
import FMCt.Evaluator
import FMCt.Examples
import FMCt.TypeChecker
import Control.Monad(void, forM_)
import Data.String (IsString(..))
import qualified Data.Map as M
import Control.Monad.ST
import Test.QuickCheck (Gen, arbitrary, sample, elements, oneof, listOf, sized, resize, vectorOf)

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
  l  <- genLocation
  return $ P t1 l t2

genAbs :: Gen Tm
genAbs = do
  b <- genBinder
  t <- genType
  l <- genLocation
  t'<- genTerm
  return $ B b t l t'

-- | Star Generator
genStar :: Gen Tm
genStar = return St

--------------------------------------------------------------------------------
-- Aux
-- | Binder Generator
genBinder :: Gen String
genBinder = vectorOf 3 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

-- | Location Generator
genLocation = oneof [genPreDefLocation, genRandLocation]

-- | Predefined Location Generator
genPreDefLocation :: Gen Lo
genPreDefLocation = elements [Out, In, Rnd, Nd, Ho, La]

-- | Random Location Generator
genRandLocation :: Gen Lo
genRandLocation = Lo <$> genBinder

--------------------------------------------------------------------------------
-- Type Generators
-- | Type Generator
genType :: Gen T
genType = undefined
--oneof [genTypeConstant, genLocationType, genHigherType]

-- -- | Constant Type Generator
-- genTypeConstant :: Gen T
-- genTypeConstant = do
--   x <- genBinder
--   return $ TConst x

-- -- | Location Type Generator
-- genLocationType :: Gen T
-- genLocationType = do
--   x <- genType
--   l <- genLocation
--   return $ TLocat l x

-- -- | Higher Type Generator
-- genHigherType :: Gen T
-- genHigherType = do
--   x <- genType
--   y <- genType
--   return $ x :=> y
