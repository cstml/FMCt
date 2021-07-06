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
import Control.Applicative

-- | TypeChecking Error.
data TypeError = SimpleErr String          -- ^ A Simple Error
               | ClashErr String           -- ^ A Type Clash
               | InadequateTypeErr Tm      -- ^ Type not High
               deriving Show

data TypeInference = TI (Either (Maybe T) TypeError)

instance Semigroup TypeInference  where
  (TI t1) <> (TI t2) = TI $ t1 <> t2

instance Monoid TypeInference where
  mempty = TI $ Left Nothing 

newtype TypeCheck a = TypeCheck {getTest :: Tm -> a}

instance Functor TypeCheck where
  fmap f (TypeCheck m) = TypeCheck $ \term -> f (m term)

instance Applicative TypeCheck where
  pure t = TypeCheck $ \term -> t
  (TypeCheck f) <*> (TypeCheck i) = TypeCheck $
    \term -> let x = i term; y = f term in  y x

instance Semigroup a => Semigroup (TypeCheck a) where
  (TypeCheck t1) <> (TypeCheck t2) = TypeCheck $ \term -> (t1 term) <> (t2 term)

instance Monoid a => Monoid (TypeCheck a) where
  mempty = TypeCheck $ \term -> mempty

typeCheck :: Tm -> Maybe T
typeCheck t =
  case tests t of
    TI (Left t) -> t
    TI (Right e) -> error $ show e
  where tests = getTest $ mconcat [ higherType ]

-- | Tests if all the terms are higher typed 
higherType :: TypeCheck TypeInference
higherType = fmap TI $ TypeCheck $ \term -> f term
  where
    err = \term -> InadequateTypeErr term
    f term =
          case term of
            V _ t --  If it is a Variable it doesn't matter
              -> (Left Nothing) <> f t
            P t _ t' --  If it is an Application it doesn't matter
              -> (Left Nothing) <> f t <> f t'
            B _ ty _ t --  If it is an Abstraction we check the term is of higher kind
              -> f t <> case ty of
                       _ :=> _ -> Left Nothing
                       _ -> Right $ err term
            St -- If it is Star we reached the end of the term
              -> Left Nothing

type TVar = String

type Context = Map Tm (Either T TVar)

type STContext a = ST a (Either T TVar)

