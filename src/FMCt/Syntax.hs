{-# LANGUAGE FlexibleInstances #-}

-- |
--Module      : Syntax
--Description : Syntax module of the FMCt.
--
--Syntax module of the FMCt.
module FMCt.Syntax (
    Lo (..),
    T,
    Tm (..),
    Type (..),
    Vv,
    module P,
) where

import FMCt.Aux.Pretty as P

type Vv = String  -- ^ Variable Value is represeted by a String.

-------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
-------------------------------------------

-- | FMC Terms Type
data Tm
    = V Vv Tm      -- ^ Variable
    | P Tm Lo Tm   -- ^ Application or Push: [M]a.N
    | B Vv T Lo Tm -- ^ Abstraction or Pop:  a\<x:t\>.N
    | St           -- ^ Star
    deriving (Eq, Show)

--------------------------------------------------------------------------
-- Location Parametrised types = out(a), in(a),b,in(c), int, in(a,b,c)) --
--------------------------------------------------------------------------

infixr 9 :=>

-- | Term Types
type T = Type String

-- | Type data structure
data Type a
  = TCon a             -- ^ Type Constant.
  | TVar a             -- ^ Type Variable - (like haskell forall - WIP)
  | TVec [Type a]      -- ^ Type Vector
  | TLoc Lo (Type a)   -- ^ Location Parametrised Type.
  | Type a :=> Type a  -- ^ A Function Type.
  | TEmp               -- ^ Empty
  deriving (Eq,Show,Ord)

instance Functor Type where
  fmap f = \case
    TEmp    -> TEmp
    TCon x  -> TCon $ f x
    TVec [] -> TEmp
    TLoc l x -> TLoc l (f <$> x)
    TVar x -> TVar $ f x
    TVec (x:xs) -> (f <$> x) <> (f <$> (TVec xs))
    x :=> y -> (f <$> x) :=> (f <$> y) 

instance Semigroup (Type a) where
    TEmp <> x = x
    x <> TEmp = x
    TVec [] <> x = x
    x <> TVec [] = x
    x <> TVec y = TVec $ x : y
    TVec x <> y = TVec $ x ++ [y]
    xx@(TCon _) <> yy@(TCon _) = TVec [xx, yy]
    xx <> yy = TVec [xx, yy]

instance Monoid T where
    mempty = TEmp 

--------------------------------------------
-- Location = {out, in, rnd, nd, x, γ, λ} --
--------------------------------------------

-- | Predefined Locations of the FMC together with general locations.
data Lo 
  = Out       -- ^ User Output Location - can only be pushed to.
  | In        -- ^ User Input Location - can only be popped from.
  | Rnd       -- ^ Rnd Input Stream - can only be popped from.
  | Nd        -- ^ Non Deterministic Stream - can only be popped from.
  | Ho        -- ^ Home stack : γ ∈ A.
  | La        -- ^ Default push Location: λ ∈ A.
  | Lo String -- ^ any other location: x ∈ A.
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Show instances
instance Show Lo where
  show x = case x of
    Out  -> "Out"
    In   -> "In"
    Rnd  -> "Rnd"
    Nd   -> "Nd"
    Ho   -> "Ho"
    La   -> "La"
    Lo y -> "Lo" ++ show y

instance Pretty Lo where
  pShow = \case
    Out -> "out"
    In -> "in"
    Rnd -> "rnd"
    Nd -> "nd"
    Ho -> "γ"
    La -> "λ"
    Lo y -> y

instance Pretty (Type String) where
    pShow x = case x of
        TCon "" -> " "
        TEmp  -> "()" 
        TCon y -> y
        TVar y -> y 
        TVec _x -> mconcat ["(", init $ mconcat $ (flip (++) ",") <$> pShow <$> _x, ")"]
        TLoc l y -> pShow l ++ "(" ++ pShow y ++ ")"
        t1 :=> t2 -> mconcat [pShow t1, " => ", pShow t2]

instance Pretty Tm where
  pShow = \case
    B v t l t' -> pShow l ++ "<" ++ v ++ ":" ++ pShow t ++ ">" ++ "." ++ pShow t'
    P t l t' -> "[" ++ pShow t ++ "]" ++ pShow l ++ "." ++ pShow t'
    V v t -> v ++ "." ++ pShow t -- untyped version
    St -> "*"

