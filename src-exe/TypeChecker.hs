module TypeChecker
  ( typeCheck
  , TypeError(..))
where

import Syntax 
import Data.List (sort)
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.ST
import Parsing
import Text.Read (readMaybe)

typeCheck :: Tm -> Either T TypeError
typeCheck = \term -> runST $ do
  return $ Left $ TConst "x"

type TVar = String

type Context = Map Tm (Either T TVar)

type STContext a = ST a (Either T TVar)

-- | TypeChecking Error.
data TypeError = SimpleEr String  -- ^ A Simple Error
               | TypeClash String -- ^ A Type Clash
               deriving Show
