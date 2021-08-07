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
) where

-- | Variable Value DataType
type Vv =
    -- | Variable Value is represeted by a String.
    String

-------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
-------------------------------------------

-- | FMC Terms Type
data Tm
    = -- | Variable
      V Vv Tm
    | -- | Application or Push: [M]a.N
      P Tm Lo Tm
    | -- | Abstraction or Pop:  a\<x:t\>.N
      B Vv T Lo Tm
    | -- | Star
      St
    deriving (Eq, Ord)

--------------------------------------------------------------------------
-- Location Parametrised types = out(a), in(a),b,in(c), int, in(a,b,c)) --
--------------------------------------------------------------------------

infixr 9 :=>

-- | Term Types
type T = Type String

-- | Type data structure
data Type a
    = -- | Type Constant.
      --            | TVar a             -- ^ Type Variable - (like haskell forall - WIP)
      TCon a
    | -- | Type Vector
      TVec [Type a]
    | -- | Location Parametrised Type.
      TLoc Lo (Type a)
    | -- | A FMC Type.
      Type a :=> Type a
    deriving (Eq, Ord)

instance Semigroup T where
    TVec [] <> x = x
    x <> TVec [] = x
    TCon "" <> x = x
    x <> TVec y = TVec $ x : y
    TVec x <> y = TVec $ x ++ [y]
    x <> TCon "" = x
    xx@(TCon _) <> yy@(TCon _) = TVec [xx, yy]
    xx <> yy = TVec [xx, yy]

instance Monoid T where
    mempty = TCon ""

--------------------------------------------
-- Location = {out, in, rnd, nd, x, γ, λ} --
--------------------------------------------

-- | Predefined Locations of the FMC together with general locations.
data Lo
    = -- | User Output Location - can only be pushed to.
      Out
    | -- | User Input Location - can only be popped from.
      In
    | -- | Rnd Input Stream - can only be popped from.
      Rnd
    | -- | Non Deterministic Stream - can only be popped from.
      Nd
    | -- | Home stack : γ ∈ A.
      Ho
    | -- | Default push Location: λ ∈ A.
      La
    | -- | any other location: x ∈ A.
      --        | InInt            -- ^ User Input Location for Ints
      --        | InBool           -- ^ User Input Location for Bools
      --        | InChar           -- ^ User Input Location for Chars
      Lo String
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Show instances
instance Show Lo where
    show x = case x of
        Out -> "out"
        In -> "in"
        Rnd -> "rnd"
        Nd -> "nd"
        Ho -> "γ"
        La -> "λ"
        Lo y -> y

instance Show (Type String) where
    show x = case x of
        TCon "" -> " "
        TCon y -> y
        TVec _x -> mconcat ["(", init $ mconcat $ (flip (++) ",") <$> show <$> _x, ")"]
        TLoc l y -> show l ++ "(" ++ show y ++ ")"
        t1 :=> t2 -> mconcat ["(", show t1, " => ", show t2, ")"]

instance Show Tm where
    show x = case x of
        B v t l t' -> show l ++ "<" ++ v ++ ":" ++ show t ++ ">" ++ "." ++ show t'
        P t l t' -> "[" ++ show t ++ "]" ++ show l ++ "." ++ show t'
        V v t -> v ++ "." ++ show t -- untyped version
        St -> "*"
