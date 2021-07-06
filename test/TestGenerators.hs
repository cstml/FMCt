module TestGenerators
  ( genTerm
  , genType
  )
where
import Syntax
import Parsing
import Pretty
import Evaluator
import Examples
import TypeChecker
import Control.Monad(void, forM_)
import Data.String (IsString(..))
import qualified Data.Map as M
import Control.Monad.ST
import Test.QuickCheck (Gen, arbitrary, sample, elements, oneof, listOf, sized, resize, vectorOf)

--------------------------------------------------------------------------------
-- Term Generators
-- | Term Generator
genTerm :: Gen Tm
genTerm = oneof [genVariables, genStar, genApp, gen_Abs]

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

gen_Abs :: Gen Tm
gen_Abs = do
  b <- genBinder
  t <- genType
  l <- genLocation
  t'<- genTerm
  return $  B b t l t'

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
genRandLocation = do Lo <$> genBinder

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
