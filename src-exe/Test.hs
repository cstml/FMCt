module Test
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
import Test.QuickCheck (Gen, arbitrary, sample, elements, oneof)

-- | Term Generator
gen_Term :: Gen Tm
gen_Term = oneof [gen_Variables, gen_Star, gen_App]

-- | Variable Generator
gen_Variables :: Gen Tm
gen_Variables = do
  let sample = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  x <- elements sample 
  t <- gen_Term
  return $ V [x] t

-- | Star Generator
gen_Star :: Gen Tm
gen_Star = return St

-- | Application Generator
gen_App :: Gen Tm
gen_App = do
  t1 <- gen_Term
  t2 <- gen_Term
  l  <- gen_Location
  return $ P t1 l t2
  
-- | Location Generator
gen_Location :: Gen Lo
gen_Location = elements [In, Out, La, Ho]
{-
-- | Type Generator
gen_Type :: Gen T
gen_Type = do
  
-}
