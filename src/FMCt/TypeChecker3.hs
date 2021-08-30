{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module FMCt.TypeChecker3 where 
import FMCt.Syntax
import FMCt.Parsing
import Control.Monad
import Control.Lens
import FMCt.Aux.Pretty (pShow,Pretty)

type Term = Tm

data Typing = Typing { _binder :: Vv
                     , _typing :: T
                     }
            deriving (Show,Eq)

type TContext = [Typing] 

data Judgement = Judgement { _tContext :: TContext
                           , _term     :: Term
                           , _termType ::  T                        
                           }
               deriving (Show,Eq)

data Derivation =
  Star          Judgement
  | Variable    Judgement 
  | Abstraction Judgement Derivation
  | Application Judgement Derivation
  | Fusion      Judgement Derivation Derivation
  deriving (Show,Eq)

makeLenses ''Judgement
makeLenses ''Typing
makePrisms ''Judgement
makePrisms ''Derivation
makePrisms ''Tm
makePrisms ''Type

derive :: Term -> Derivation
derive t = Star $ Judgement [] t TEmp

