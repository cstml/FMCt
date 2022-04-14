{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module FMCt.TypeChecker (
    -- ** Types 
    Derivation (..),
    Judgement,
    TSubs,

    -- ** Lens 
    judgement,
) where

import Control.Exception
import Control.Monad
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set
import FMCt.Aux.Pretty (Pretty, pShow)
import FMCt.Parsing
import FMCt.Syntax
import FMCt.TypeChecker.Aux
import FMCt.TypeChecker.Error
import qualified Data.Map as Map
import Control.Lens
import Control.Arrow ((>>>))

-- | Standard Derivation type. Based on the FMC's BNF.
data Derivation
    = Star        !Judgement
    | Variable    !Judgement !Derivation
    | Abstraction !Judgement !Derivation
    | Application !Judgement !Derivation !Derivation
    deriving (Show, Eq)

makePrisms ''Derivation

judgement :: Lens' Derivation Judgement 
judgement =
  let
    getter = \case
      Star j -> j
      Variable j _ -> j
      Abstraction j _ -> j
      Application j _ _  -> j
    setter s r = case s of
      Star _ -> Star r
      Variable _ d -> Variable r d
      Abstraction _ d -> Abstraction r d
      Application _ d d' -> Application r d d'
  in lens getter setter

-- | Empty TypingContext
emptyCx :: TypingContext
emptyCx = let star = ("*", mempty :=> mempty) in Map.fromList [star]

termToDerivation :: Term -> Derivation
termToDerivation term = undefined
{-  let
    a term' = Judgement emptyCx term'
  in  \case
   V v tm  -> Star (a term)
   P t l tm -> Star (a term)
   B v ty l tm -> Star (a term)
   St -> Star (a term)
-}
