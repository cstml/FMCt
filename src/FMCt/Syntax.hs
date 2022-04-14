{-# LANGUAGE FlexibleInstances, DerivingVia, DerivingStrategies, GeneralizedNewtypeDeriving, TemplateHaskell #-}

-- |
-- Module      : Syntax
-- Description : Syntax module of the FMCt.
--
-- Syntax module of the FMCt.
module FMCt.Syntax (
    Lo (..),
    T(..),
    Tm (..),
    Vv,
    TVariable,
    TConstant,
    tConstant,
    tVariable,
    module P,
) where

import Control.Lens 
import FMCt.Aux.Pretty as P
import Data.String (IsString, fromString)

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
    deriving (Eq, Ord, Show)

-- | Variable Value is represeted by a String.
type Vv = String

newtype TConstant = TConstant {_tConstant :: String}
  deriving newtype (Show, Eq, Ord)
makeLenses ''TConstant

instance IsString TConstant where
  fromString = review tConstant

newtype TVariable = TVariable {_tVariable :: String}
  deriving newtype (Show, Eq, Ord)
makeLenses ''TVariable

instance IsString TVariable where
  fromString = review tVariable

infixr 9 :=>

  -- | FMCt Term Types
data T
    = -- | Type Constant.
      TCon TConstant
    | -- | Type Variable.
      TVar TVariable
    | -- | Type Vector.
      TVec [ T ]
    | -- | Location Parametrised Type.
      TLoc Lo T
    | -- | A Function Type.
      T :=> T
    | -- | Empty Type.
      TEmp
    deriving (Eq, Show, Ord)

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
  x <> y =
    case x of
      V vv t' -> V vv (t' <> y)
      P tm lo t' -> P tm lo (t' <> y)
      B vv t lo t' -> B vv t lo (t' <> y)
      St -> y

instance Monoid Tm where
  mempty = St

--------------------------------------------------------------------------------
-- Show instances

instance Pretty Lo where
    pShow = \case
        Out -> "out"
        In -> "in"
        Rnd -> "rnd"
        Nd -> "nd"
        Ho -> "γ"
        La -> "λ"
        Lo y -> y

tVB :: (String,String)
tVB = ("[","]")

instance Pretty T where
    pShow x = case x of
        TCon "" -> " "
        TEmp -> "()"
        TCon y ->  y ^. tConstant
        TVar y -> y ^. tVariable
        TVec y ->
          let
          go = \case
            TVec [x] -> pShow x
            TVec [] -> ""
            TVec (x:xs) -> pShow x <> " ; " <> go (TVec xs)
          in
          fst tVB <> go x <> snd tVB
        TLoc l y -> "@" ++ pShow l ++ "(" ++ pShow y ++ ")"
        t1 :=> t2 -> mconcat ["{", pShow t1, " => ", pShow t2, "}" ]

instance Pretty Tm where
    pShow =
      let
        sep = ";"
      in
      \case
        B v t l t' -> mconcat [pShow l, "<", v," : ", pShow t,">",";",pShow t']
        P t l t' -> "[" ++ pShow t ++ "]" ++ pShow l ++ sep ++ pShow t'
        V v t -> v ++ sep ++ pShow t
        St -> "*"
