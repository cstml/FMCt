{-# LANGUAGE TypeOperators #-}
module Syntax
  ( T(..)
  , VT(..)
  , TT(..)
  , Tm(..)
  , Vv(..)
  , Lo(..)
  , K(..)
  , emptyT
  )
where
import           Data.Monoid 
import           Data.Monoid    (Monoid, (<>), mempty, mconcat)
import           Data.Semigroup (Semigroup)
import           Data.Traversable
import           Data.Functor
import qualified Data.Map  as M
import           Data.Map  (Map, (!?))

-- | Variable Value Type 
type Vv = String            -- ^ Variable Value

-------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
-------------------------------------------

-- | FMC Terms Type
data Tm = V Vv Tm          -- ^ Variable
        | P Tm Lo Tm       -- ^ Application or Push [M]a.N
        | B Vv TT Lo Tm    -- ^ Abstraction or Pop  a<x:t>.N
        | E Tm Tm          -- ^ Evaluate Term
        | St               -- ^ Star 
        deriving (Eq)

--------------------------------------------------------------------------
-- Location Parametrised types = out(a), in(a),b,in(c), int, in(a,b,c)) --
--------------------------------------------------------------------------

-- | Type Kinds
data K a = K a             -- ^ Value Kind
         | K a :=> K a     -- ^ Higher Kind
         deriving Eq


type VT a = [a]         -- ^ a vector of types

-- | Location Types
data T a = T Lo a
        deriving Eq

-- | FMC Types
type TT = K (VT (T String))

emptyT = K []

--------------------------------------------
-- Location = {out, in, rnd, nd, x, γ, λ} --
--------------------------------------------
-- | Predefined Locations of the FMC together with general locations.
data Lo = Out              -- ^ User Output Location
        | In               -- ^ User Input Location
        | Rnd              -- ^ Rnd Input Stream
        | Nd               -- ^ Non Deterministic Stream
        | Ho               -- ^ Home stack : γ ∈ A
        | La               -- ^ Default push Location:   λ ∈ A
        | Lo  String       -- ^ any other location: x ∈ A
        deriving (Eq, Ord)
  
--------------------------------------------------------------------------------
-- Show instances
instance Show Lo where
  show x = case x of
    Out  -> "out"
    In   -> "in"
    Rnd  -> "rnd"
    Nd   -> "nd"
    Ho   -> ""--"γ"
    La   -> "λ"
    Lo x -> x
  
instance Show Tm where
  show x = case x of
    B v t l t' -> show l ++ "<" ++ v ++ ":" ++ show t ++ ">" ++ "." ++ show t'
    P t l t'   -> "[" ++ show t ++ "]" ++ show l ++ "." ++ show t'
    V v t      -> v ++ "." ++ show t -- untyped version
--    V v tt t   -> v ++ ":" ++ show tt ++  "." ++ show t -- typed version
    St          -> "*"

instance (Show a) => Show (T a) where
  show (T l t) = show l ++ "(" ++ show t ++  ")"
{-
instance (Show a) => Show (VT a) where
  show x = case x of
    VT x    -> "(" ++ show x ++ ")"
-}
instance (Show a) => Show (K a) where
  show x = case x of
    (K x)   -> "(=>" ++ show x ++ ")"
    x :=> y -> "(" ++ show x ++ show y ++ ")"

