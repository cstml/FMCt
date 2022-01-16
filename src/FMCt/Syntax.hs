{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Syntax
-- Description : Syntax module of the FMCt.
--
-- Syntax module of the FMCt.
module FMCt.Syntax
    ( Lo (..),
      T,
      Tm (..),
      Type (..),
      Vv,
      module P,
    )
where

import FMCt.Aux.Pretty as P

type Vv =
    -- | Variable Value is represeted by a String.
    String

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
    deriving (Eq, Show)

instance Semigroup Tm where
    (<>) = \case
        St -> id
        V vv t -> \t' -> V vv (t <> t')
        P tt l t -> \t' -> P tt l (t <> t')
        B vv tt lo t -> \t' -> B vv tt lo (t <> t')

instance Monoid Tm where
    mempty = St

infixr 9 :=>

-- | FMCt Term Types
type T = Type String

-- | Type data structure
data Type a
    = -- | Type Constant.
      TCon a
    | -- | Type Variable.
      TVar a
    | -- | Type Vector.
      TVec [Type a]
    | -- | Location Parametrised Type.
      TLoc Lo (Type a)
    | -- | A Function Type.
      Type a :=> Type a
    | -- | Empty Type.
      TEmp
    deriving (Eq, Show, Ord)

instance Functor Type where
    fmap f = \case
        TEmp -> TEmp
        TCon x -> TCon $ f x
        TVec [] -> TEmp
        TLoc l x -> TLoc l (f <$> x)
        TVar x -> TVar $ f x
        TVec (x : xs) -> (f <$> x) <> (f <$> (TVec xs))
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
      Lo String
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Show instances
instance Show Lo where
    show x = case x of
        Out -> "Out"
        In -> "In"
        Rnd -> "Rnd"
        Nd -> "Nd"
        Ho -> "Ho"
        La -> "La"
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
        TEmp -> "()"
        TCon y -> y
        TVar y -> y
        TVec _x -> mconcat ["(", init $ mconcat $ (flip (++) ",") <$> pShow <$> _x, ")"]
        TLoc l y -> pShow l ++ "(" ++ pShow y ++ ")"
        t1 :=> t2 -> mconcat [pShow t1, " => ", pShow t2]

instance Pretty Tm where
    pShow = \case
        B v t l t' -> pShow l ++ "<" ++ v ++ ":" ++ pShow t ++ ">" ++ "." ++ pShow t'
        P t l t' -> "[" ++ pShow t ++ "]" ++ pShow l ++ "." ++ pShow t'
        V v t -> v ++ "." ++ pShow t
        St -> "*"
