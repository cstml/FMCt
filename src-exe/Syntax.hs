{-|
Module      : Syntax
Description : Short description

Syntax module of the FMCt.
-}
module Syntax
  ( T(..)
  , Tm(..)
  , Vv(..)
  , Lo(..)
  , TConstant
  , Type(..)
  )
where

-- | Variable Value DataType 
type Vv = String            -- ^ Variable Value is represeted by a String.

-------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
-------------------------------------------

-- | FMC Terms Type
data Tm = V Vv Tm          -- ^ Variable
        | P Tm Lo Tm       -- ^ Application or Push: [M]a.N
        | B Vv T  Lo Tm    -- ^ Abstraction or Pop:  a\<x:t\>.N
        | E Tm Tm          -- ^ Evaluate Term -- might drop
        | St               -- ^ Star 
        deriving (Eq, Ord)

--------------------------------------------------------------------------
-- Location Parametrised types = out(a), in(a),b,in(c), int, in(a,b,c)) --
--------------------------------------------------------------------------

infixr 9 :=>

-- | Type Constants
type TConstant = String -- ^ Type constants are strings, with void being represented by `""` or empty String.

-- | Location Types are a Vector of Kinded FMCt Location parametrised type constants. 
type T = Type TConstant

data Type a = TConst a  -- ^ Type Constant.
            | TLocat Lo (Type a) -- ^ Location Parametrised Type.
            | Type a :=> Type a -- ^ A FMC Type.
            | TVector [Type a] -- ^ A Vector Type
            deriving (Eq, Ord)

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

instance (Show a) => Show (Type a) where
  show x = case x of
    TConst x -> show x
    TLocat l x -> show l ++ "(" ++ show x ++ ")"
    t1 :=> t2 -> show t1 ++ " => " ++ show t2
    TVector x -> "(" ++ (mconcat $ (++ ", ") . show <$> x) ++ ")"
  
instance Show Tm where
  show x = case x of
    B v t l t' -> show l ++ "<" ++ v ++ ":" ++ show t ++ ">" ++ "." ++ show t'
    P t l t'   -> "[" ++ show t ++ "]" ++ show l ++ "." ++ show t'
    V v t      -> v ++ "." ++ show t -- untyped version
--    V v tt t   -> v ++ ":" ++ show tt ++  "." ++ show t -- typed version
    St          -> "*"
