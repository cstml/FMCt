module Syntax
  ( VT(..)
  , T(..)
  , Tm(..)
  , Vv(..)
  , Lo(..)
  , K(..)
  , GLT(..)
  , FMCLT
  , FMCVt
  , emptyT
  , TConstant
  )
where

-- | Variable Value DataType 
type Vv = String            -- ^ Variable Value is represeted by a String.

-------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
-------------------------------------------

-- | FMC Terms Type
data Tm = V Vv Tm          -- ^ Variable
        | P Tm Lo Tm       -- ^ Application or Push [M]a.N
        | B Vv T  Lo Tm    -- ^ Abstraction or Pop  a<x:t>.N
        | E Tm Tm          -- ^ Evaluate Term
        | St               -- ^ Star 
        deriving (Eq)

--------------------------------------------------------------------------
-- Location Parametrised types = out(a), in(a),b,in(c), int, in(a,b,c)) --
--------------------------------------------------------------------------

infixr 9 :=>

-- | Generic datatype for Type Kinds
--
-- As a general note, inside the FMTt there is no Value Kind as each term must
-- be of the type a => b. The closest we have to a constant an element of the
-- type =>a.
data K a = K a             -- ^ Value Kind - it cannot exist by itself inside the FMCt.
         | K a :=> K a     -- ^ Higher Kind - representing the evaluation of an FMCt term. 
         deriving Eq

-- | Generic Vector Types 
type VT a = [a]            -- ^ a vector can be represented by a list of instances of type a.

-- | Type Constants
type TConstant = String -- ^ type constants are strings.

-- | FMCt Vector Types.
type FMCVt = VT TConstant -- ^ A vector of Type Constants. 

-- | Generic Location Types
data GLT a = T Lo a -- ^ Type is formed from a type constant and a location 
        deriving Eq

-- | FMCt Location Type
type FMCLT = GLT FMCVt

-- | Location Types are a Vector of Kinded FMCt Location parametrised type constants. 
type T = VT (K FMCLT)

-- | FMC empty Type is represented by e=>e or empty to empty
emptyT = K [] :=> K []

--------------------------------------------
-- Location = {out, in, rnd, nd, x, γ, λ} --
--------------------------------------------
-- | Predefined Locations of the FMC together with general locations.
data Lo = Out              -- ^ User Output Location - can only be pushed to.
        | In               -- ^ User Input Location - can only be popped from.
        | Rnd              -- ^ Rnd Input Stream - can only be popped from.
        | Nd               -- ^ Non Deterministic Stream - can only be popped from.
        | Ho               -- ^ Home stack : γ ∈ A.
        | La               -- ^ Default push Location: λ ∈ A.
        | Lo  String       -- ^ any other location: x ∈ A.
        deriving (Eq, Ord)
  
--------------------------------------------------------------------------------
-- Show instances
instance Show Lo where
  show x = case x of
    Out  -> "out"
    In   -> "in"
    Rnd  -> "rnd"
    Nd   -> "nd"
    Ho   -> "γ"
    La   -> "λ"
    Lo x -> x
  
instance Show Tm where
  show x = case x of
    B v t l t' -> show l ++ "<" ++ v ++ ":" ++ show t ++ ">" ++ "." ++ show t'
    P t l t'   -> "[" ++ show t ++ "]" ++ show l ++ "." ++ show t'
    V v t      -> v ++ "." ++ show t -- untyped version
--    V v tt t   -> v ++ ":" ++ show tt ++  "." ++ show t -- typed version
    St          -> "*"

instance (Show a) => Show (GLT a) where
  show (T l t) = show l ++ "(" ++ show t ++  ")"

instance (Show a) => Show (K a) where
  show x = case x of
    K x     -> "(" ++ show x ++ ")"
    x :=> y -> "(" ++ show x ++ "->" ++ show y ++ ")"
